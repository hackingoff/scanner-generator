module ThompsonConstruction
  PENDING = 0;
#############################################################################
# Thompson-McNaughton-Yamada Construction Section
#############################################################################
  def build_machine_stack(re)
    skip = 0
    escaped = false
    machines = Array.new
    (0...re.length).each do |ii| # the pointer in some cases.
      (skip -= 1) && next if skip != 0 # Advance ptr until past () group
      ch = re[ii] #re[-ii-1]
      if escaped
        case ch
        when 'n'
          machines.push([cat_machine("\n"), nil])
        else
          machines.push([cat_machine(ch), nil])
        end
        escaped = false
        next
      end
      case(ch)
      when '*' then machines.push([kleene_machine, [1,2]])
      when '+' then machines.push([plus_machine, [1,2]])
      #when '+' then machines.push([plus_machine, [[0,1],[1,1]]])
      when '?' then machines.push([question_machine, [0,1]])
      when '|' then machines.push([alt_machine, [1,2,3,4]])
      when ']' then raise "mismatched bracket closed a non-open class"
      when ')' then raise "mismatched paren closed a non-open group"
      when '('# ; puts "#{ms}\tGRPOPEN\nencounted closing paren. following chars #{re[ii+1]}#{re[ii+2]}" 
        subexpression = ''
        nesting = 0
        until (ch2 = re[ii+=1]) == ')' && nesting == 0 # Until the next character is '('
          nesting -= 1 if ch2 == ')'
          nesting += 1 if ch2 == '('
          subexpression << ch2
          #skip += 1
        end
        #skip += 1
        subgraph = re2nfa(subexpression)
        skip = subexpression.length+1 # the +1 is used to skip the closing )
        machines.push([subgraph, nil])
      when '['
        char_class = get_char_class(re[ii..-1]) # search rest of the string for []-expression
        machines.push([cat_machine(/#{char_class}/), nil])
        skip = char_class.length - 1 + char_class.scan(/\\/).length # compensate for 2 '\'s counting as 1
        # The below skip assignment works if we want to allow for odd numbers of slashes, but it's
        # not desirable, because it would allow [\n] to be [n].
        # We're reserving \ for escaping *, +, ?, etc. symbols.
        #skip = char_class.length - 1 +
        #       char_class.scan(/\\/).length*2 -
        #       char_class.scan(/\\\\/).length # compensate for 2 '\'s counting as 1
      when '\\' #; escaped = true unless escaped== true
        if escaped  # '\\' -> cat a slash
          machines.push([cat_machine(ch), nil])
          escaped = false
        else
          escaped = true
        end
      else
        machines.push([cat_machine(ch), nil])
      end
    end
    machines
  end

  def get_char_class(str)
    escaped = false
    result = ''

    str.each_char.with_index do |ch,ii|
      if escaped == false && ch == ']' # done reading current class
        result += ch
        return result
      elsif escaped == true
        result = result[0..-2]+ch
      else
        result += ch
      end
      escaped = (ch == '\\' && escaped==false)    
    end
    raise 'character class improperly closed!'
  end

  def kleene_up(machines)
    new_machines = Array.new
    machines.each_with_index do |mach,ii|
      if mach[1].nil? || mach[1].empty? # This machine is complete.
        new_machines.push([mach[0],nil])
      else
        if mach[1].length == 2 # Deals with *, ?, and +, who all have same precedence
          src, dest = mach[1].shift, mach[1].shift
          #m = mach[0].lambda_replace_edge(src,PENDING,dest,new_machines.pop) # LAMBDA VERSION
          m = mach[0].replace_edge(src,PENDING,dest,new_machines.pop[0]) # NON-LAMBDA VERSION
          new_machines.push([m,nil])
        else # dealing with |
          new_machines.push([mach[0],mach[1]])
        end
      end
    end
    new_machines
  end

  def catify(machines)
    new_machines = Array.new
    machines.each_with_index do |mach,ii|
      if ii == 0
        new_machines.push([mach[0],nil])
      elsif (mach[1].nil? && machines[ii-1][1].nil?)
          # This machine AND PREVIOUS are each a cat or finished */?/+
          # This code is suspiciously similar to the wrap-up code of re2nfa()
          # which implies that it's not DRY. This is something to revisit.
          lead = new_machines.pop[0]
          offset = lead.get_node_count-1
          acc = lead.accept_states.keys.first || 0
          lead.imp_attach_graph(acc,mach[0])
          lead.accept_states.delete_if do |acc_st|
            !mach[0].accept_states.keys.include?(acc_st-offset)
          end
          new_machines.push([lead,nil])
      else
        new_machines.push([mach[0],mach[1]])
      end
    end
    new_machines
  end

  def handle_alternation(machines)
    machines = absorb_left_alt(machines)
    machines = absorb_right_alt(machines)
  end

  def absorb_left_alt(machines)
    new_machines = Array.new
    machines.each_with_index do |mach,ii|
      if mach[1].nil? || mach[1].empty? # This machine is complete.
        new_machines.push([mach[0],nil])
      else
        src, dest = mach[1].shift, mach[1].shift
        m = mach[0].replace_edge(src,PENDING,dest,new_machines.pop[0]) # NON-LAMBDA VERSION
        new_machines.push([m,mach[1]])
      end
    end
    new_machines
  end

  def absorb_right_alt(machines)
    absorb_left_alt(machines.reverse).reverse
  end

  # This is a Thompson construction of a regular expression to a NFA.
  # The machine stack is a series of 2-tuples. The first element of which
  # is a small NFA, the second of which is a listing of the edges it needs
  # to fill in by cannibalizing an adjacent NFA.

  # mptr = machines.length - 1 # machine index pointer
  # m = machines[mptr]
  # * eats below IF below complete
  # | eats above and below if they're complete

  # make one pass forwards, completing all kleene stars and all alt LHSs
  # make one pass backwards, completing all alt RHSs
  # if any unfulfilled dependencies remain, my assumptions were mistaken
  def re2nfa(re)
    #puts "re2nfa: #{re}"
    fsconstruct = FiniteStateMachine.new({:accept_states => {0=>'eh'},
                                  :graph_hash    => {0=>{PENDING=>[0]}}})
    machines = build_machine_stack(re)
    machines = kleene_up(machines)
    machines = catify(machines)
    machines = handle_alternation(machines)

    #puts "New machines:"
    machines.each_with_index do |mach,ii|
      m = mach[0]
      offset = fsconstruct.get_node_count-1
      acc = fsconstruct.accept_states.keys.first || 0 # Attachment point is accept state
      fsconstruct.imp_attach_graph(acc, m)
      fsconstruct.accept_states.delete_if do |acc_st|
        #puts "purging acc #{acc}" if !m.accept_states.keys.include?(acc-offset)
        !m.accept_states.keys.include?(acc_st-offset)
      end
    end

    fsconstruct.delete_edge(0,PENDING,0)
    #@graph_hash = fsconstruct.graph_hash
    #@accept_states = fsconstruct.accept_states
    FiniteStateMachine.new({
      :graph_hash   => fsconstruct.graph_hash,
      :accept_states => fsconstruct.accept_states
      })
  end

  def set_new_accept(node_number, type='end')
    @accept_states = {node_number => 'end'}
  end

  def prepend_graph(fsm)
    fsm.imp_attach_graph(fsm.accept_states.keys[0],self)
    copy(fsm)
  end

  def cat_machine(ch)
    FiniteStateMachine.new({
      :accept_states => {1=>'end'},
      :graph_hash    => {0 => {ch => [1]}}
    })
  end

  def question_machine
    FiniteStateMachine.new({
      :accept_states => {1=>'end'},
      :graph_hash    => {0 => {PENDING => [1], LAMBDA => [1]}}
      #:accept_states => {3=>'end'},
      #:graph_hash => {
      #  0 => {LAMBDA => [1,3]},
      #  1 => {PENDING => [2]},
      #  2 => {LAMBDA => [3]}
      #}
    })
  end

  def alt_machine
    FiniteStateMachine.new({
      :accept_states => {5=>'end'},
      :graph_hash    => {
        0 => {LAMBDA => [1,3]},
        1 => {PENDING => [2]},
        2 => {LAMBDA => [5]},
        3 => {PENDING => [4]},
        4 => {LAMBDA => [5]}
      }
    })
  end

  def kleene_machine
    FiniteStateMachine.new({
      :accept_states => {3=>'end'},
      :graph_hash    => {
        0 => {LAMBDA => [1,3]},
        1 => {PENDING => [2]},
        2 => {LAMBDA => [1,3]}
      }
    })
  end

  def plus_machine
    FiniteStateMachine.new({
      :accept_states => {3=>'end'},
      :graph_hash    => {
        0 => {LAMBDA => [1]},
        1 => {PENDING => [2]},
        2 => {LAMBDA => [1,3]}
      }
      # The below machine would be more concise, but we'd need to add in logic to replace TWO EDGES with one absorb.
      #:accept_states => {1=>'end'},
      #:graph_hash => {
      #  0 => {PENDING => [1]},
      #  1 => {PENDING => [1]}
      #}
    })
  end

#############################################################################
# Misc functions primarily supporting re2nfa
#############################################################################
    # graph edges going from origin become outgoing from src
    # graph edges going TO   final state instead go TO dest
    #   What is "final state?" Any accept state?
    #@graph_hash[src][label].delete(dest)
  def replace_edge(src, label, dest, graph)
    raise "can't inject a graph that had no accept states" if graph.accept_states.nil? || graph.accept_states.empty?
    if @graph_hash[src][label].class == Fixnum
      @graph_hash[src][label] = [@graph_hash[src][label]]
    end

    offset = get_node_count-1
    imp_attach_graph(src, graph)

    #draw_graph('intermediate-self')
    #graph.draw_graph('intermediate-graft')

    # for each of the edges pointing at the accept state of the graph
    # redirect them to point at dest
    #draw_graph('retarget-pre')
    graph.accept_states.keys.each do |acc|
      retarget_edges(acc+offset,dest)
      accept_states.delete(acc+offset)
    end
    delete_edge(src,label,dest)

    renumber!
    #draw_graph('retarget-post')

    self
  end

  # ensure no gaps in our node names!
  def renumber!
    get_node_names.each_with_index do |n,ii|
      if n != ii
        retarget_edges(n,ii)
        @accept_states[ii] = @accept_states.delete(n) unless @accept_states[n].nil?
        @graph_hash[ii] = @graph_hash.delete(n)
      end
    end
    self
  end

  # imp_attach_graph: increments fsm's node numbers by 1-CALLER.node_count
  #   takes edges outgoing from fsm.origin and adds them to attach_point
  def imp_attach_graph(attach_point, fsm)
    my_node_count = get_node_count
    graft = fsm.clone
    graft.increment_node_labels(my_node_count-1) # prevent collisions
    
    graft_root_edges = graft.graph_hash.delete(graft.origin)
    @graph_hash[attach_point] ||= Hash.new
    @graph_hash[attach_point].merge!(graft_root_edges)

    @accept_states.merge!(graft.accept_states)
    @graph_hash.merge!(graft.graph_hash)
    get_node_count
  end

  def retarget_edges(old_dest, new_dest)
    @graph_hash.each_pair do |node,edge_hash|
      edge_hash.each_pair do |label, dest|
        if dest.include? old_dest
          #puts "#{node}[#{label}] changed from #{dest} to #{new_dest}"
          add_edge(   node, label, new_dest)
          delete_edge(node, label, old_dest)
        end
      end
    end
    self
  end

  def lambda_replace_edge(src, label, dest, graph)
    if @graph_hash[src][label].class == Fixnum
      @graph_hash[src][label] = [@graph_hash[src][label]]
    end
    #@graph_hash[src][label].delete(dest)
    lambda_inject_graph(graph,src,dest)
    delete_edge(src,label,dest)
    self
  end

  def lambda_inject_graph(graph, src, dest)
    old_node_count = get_node_count
    lambda_attach_graph(src, graph)
    graph.accept_states.keys.each {|k| add_edge(k+old_node_count, LAMBDA, dest)}
    graph.accept_states.keys.each {|k| @accept_states.delete(k+old_node_count)}
    self
  end

  def clone
    Marshal.load( Marshal.dump(self) )
  end
=begin
  def add_edge(src, label, dest)
    @graph_hash[src] = Hash.new if @graph_hash[src].nil?
    if @graph_hash[src][label].nil?
      @graph_hash[src][label] = [dest]
    else
      if @graph_hash[src][label].class != Array
        @graph_hash[src][label] = [@graph_hash[src][label]]
      end
      @graph_hash[src][label] << dest if !@graph_hash[src][label].include?(dest)
    end
    self
  end

  # Fail silently on deleting stuff that doesn't exist.
  def delete_edge(src, label, dest)
    return self if @graph_hash[src].nil?
    return self if @graph_hash[src][label].nil?
    @graph_hash[src][label].reject! {|node| node==dest}
    if @graph_hash[src][label].empty?
      @graph_hash[src].delete(label)
    end
    # !!! may need to add something to delete orphaned nodes, here
    self
  end
=end
end