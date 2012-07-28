require 'graphviz'
require 'set'

module ScannerGenerator
  require File.dirname(__FILE__) + '/thompson_construction.rb'

  LAMBDA = "LAMBDA"; SOURCE = 0; DEST = 1; LABEL = 2;
  ERROR = 0; MACHINE_ACCEPT = 1; HALT_RETURN = 2 # Action Table codes [E, MA, HR]
  ACC = 3; WIDTH = 3

  # True if the needle (subset) is found in the haystack (superset).
  def subset(needle,haystack)
    a = needle.sort
    b = haystack.sort
    ii = 0
    jj = 0
    a_last_index = a.length-1
    b_last_index = b.length-1
    loop do
      if(a[ii]==b[jj])
        return true if(ii==a_last_index)
        ii+=1
        jj+=1
      elsif(a[ii] > b[jj])
        return false if(jj>=b_last_index)
        jj+= 1
      else # a[ii] < b[jj]
        return false
      end
    end
  end

  # is needle contained in a haystack?
  def subset_of_list_element?(needle,list_of_haystacks)
    list_of_haystacks.each{|haystack| return true if subset(needle,haystack)}
    return false
  end

  class FiniteStateMachine
    include ThompsonConstruction

    attr_reader :graph_hash, :accept_states, :origin
    attr_accessor :labels
    
    # Must set @accept_states, @edges, @edge_labels, @node_labels, @graph_hash
    # edge/node labels are derived from @graph_hash
    def initialize(input)
      raise "Bunk input" if input[:accept_states].nil? || input[:graph_hash].nil?
      @accept_states = input[:accept_states]
      @graph_hash = input[:graph_hash]
      @origin = input[:origin] || 0
      @labels = input[:labels] || {}
      @edge_labels = get_edge_labels
      @rankdir = input[:rankdir] || "TB" # TB is top-to-bottom; LR is left-to-right
      self
    end

    def copy(graph)
      @accept_states = graph.accept_states
      @graph_hash = graph.graph_hash
      @origin = graph.origin
      @edge_labels = get_edge_labels
      self
    end

    # Regex keys specify that any matching edge labels transition to the dest
    # node.
    # Example:
    # Suppose our language = {a,b,1,\n,*,/}.
    # An edge labeled "[^\n\*\/]" matches anything but newline, *, or /.
    # This function replaces that edge with multiple edges from the
    # language's alphabet.  In this case, the [^\n\*\/] edge gets replaced by
    # 3 edges: an "a" edge, a "b" edge, & a "1" edge.
    # NOTE: Invoke this AFTER drawing dfa, but BEFORE dumping the module.
    def expand_regex_edges
      #puts "Expanding regex edges..."
      @graph_hash.each_pair do |source, edge_dest_hash| # state, hash(edge=>state)
        new_edges_for_same_destination = Hash.new
        #puts "before: @graph_hash[#{source}] #{@graph_hash[source]}" if 
        edge_dest_hash.each_pair do |regex_edge, dest| # e.g. /[^\n] => 98
          next if regex_edge.class != Regexp
          #puts "before: @graph_hash[#{source}][#{edge}] = #{@graph_hash[source][edge]}"
          
          for label in @edge_labels
            if label.class == String && label.match(regex_edge)
              # unless clause prevents sloppy regex from overwriting other edges
              new_edges_for_same_destination[label] = dest unless @graph_hash[source].key?(label)
              # new_edges_for_same_destination[label] ||= dest
            end
          end
          @graph_hash[source].delete(regex_edge) # remove old regex edge
          #puts "after: @graph_hash[#{source}][#{edge}] = #{@graph_hash[source][edge].class}"
        end
        #puts @new_edges_for_same_destination.to_s
        unless new_edges_for_same_destination.empty?
          @graph_hash[source].merge!(new_edges_for_same_destination)
          #puts "after:  @graph_hash[#{source}] #{@graph_hash[source]}"
        end
      end
      self
    end

    def get_node_names
      names = @graph_hash.keys # Ensure source nodes represented.
      @graph_hash.each_pair do |source_node, sub_hash|
        names << source_node
        names << sub_hash.values
      end
      #names.flatten.map {|n| n.to_s}.uniq.sort
      names.flatten.uniq.sort
    end
    
    def get_edge_labels
      aggregate_keys = []
      @graph_hash.values.each {|sub_hash| aggregate_keys << sub_hash.keys }
      aggregate_keys.flatten.uniq
    end

    def subsetify(start_node = @origin)
      new_graph_hash = {}
      new_accept_states = {}
      new_labels = {}

      states = [closure_of(start_node)] # if passed a start node as an int, this will fail without .to_s

      edge_labels = get_edge_labels
      
      states.each do |state|
        new_graph_hash[state] = {}
        edge_labels.each do |label|
          next if label == LAMBDA
          closures_via_label = []
          
          state.each do |node|
            next if (@graph_hash[node].nil? || @graph_hash[node][label].nil?)
            found_closure = closure_of(@graph_hash[node][label])
            closures_via_label << found_closure if !closures_via_label.include?(found_closure)
          end
          
          next if closures_via_label == []
          closures_via_label.flatten!
          new_graph_hash[state][label] = closures_via_label
          states << closures_via_label unless states.include?(closures_via_label)
        end

        new_accept_states[state] = accept_state_of(state) if accept_state_of(state) != false
        new_graph_hash.delete(state) if new_graph_hash[state] == {}
        #new_labels[state]
      end

      #puts "New graph shit:"
      #ap new_graph_hash
      #ap new_accept_states
      #ap new_labels

      # THIS IS THE NEW PART FOR LABELS
      states.each do |state| 
        label = ""
        state.each do |substate|
          label << @labels[substate] + "\n" unless @labels[substate].nil? || label.include?(@labels[substate])
        end
        new_labels[state] = label.chomp unless label == ""
      end
      # END NEW PART FOR LABELS

      return FiniteStateMachine.new({
        :graph_hash=>new_graph_hash,
        :accept_states=>new_accept_states,
        :labels => new_labels}
        ).beautify
    end
    
    def subsetify!(start_node_label = 0)
      dfa = subsetify(start_node_label)
      @graph_hash,@accept_states,@labels = dfa.graph_hash, dfa.accept_states, dfa.labels
      return self
    end
    
    def draw_graph(filename = "output", svgname = :Finite_Automata_Graph, shape = "circle", path = nil)
      graph = GraphViz::new(:Finite_Automata_Graph)
      graph[:rankdir] = @rankdir
      # !!! going to have to check of @labels[node_num] (label for node # node_num exists and specify it with :label => @label[node_num] when present)
      get_node_names.each do |node|
        label = @labels[node] || node.to_s #((@labels[node].nil?) ? node.to_s : @labels[node])
        is_accept = @accept_states.include?(node)
        graph.add_nodes(node.to_s,
          :shape => shape,
          :label => label,
          :peripheries => ((is_accept) ? 2 : 1),
          :color => ((is_accept && shape == "Mrecord") ? "#66DD66" : "#000000"))
      end
      
      @graph_hash.each_pair do |source_label,sub_hash|
        sub_hash.each_pair do |edge_label,destination_nodes|
          [destination_nodes].flatten.each do |dest_label| # ensure d_n is 1-d array
            source_node = graph.get_node(source_label.to_s)
            dest_node = graph.get_node(dest_label.to_s)
            graph.add_edges(source_node, dest_node, :label => label_friendly(edge_label).gsub('\\','\\\\\\\\'))
          end
        end
      end

      unless path.nil?
        graph.output(:svg => "#{filename}.svg", :path => path)
      else
        graph.output(:svg => "#{filename}.svg")
      end
    end

    def draw_state_labeled_graph(filename = "output", svgname = :Finite_Automata_Graph, shape = "circle", path = nil)
      labels = @labels.dup
      # modify the labels
      @labels.each_with_index do |label, ii|
        lines = ""
        #ap label
        label[1].each_line {|i| lines << "<tr><td align=\"left\">#{i}</td></tr>"}
        table_border_color = (@accept_states.include?(ii)) ? "#448844" : "#ffffff"
        heading = "State #{ii}"
        @labels[ii] = '<<table color="'+table_border_color+'" style="ROUNDED" border="1" cellborder="0" cellpadding="5"><tr><td align="center" colspan="1"><font color="#666666" point-size="8">'+heading+'</font></td></tr>'+lines+'</table>>'
      end
      result = draw_graph(filename, svgname, shape, path)
      @labels = labels
      result
    end

    # adapted from class notes
    def closure_of(node_label)
      closure = [node_label].flatten
      changed = true
      while (changed == true)
        changed = false
        closure.each do |node|
          # if there is m not already in C and n->lambda->m then add m to c
          if(!@graph_hash[node].nil? && !@graph_hash[node][LAMBDA].nil?)
            lambda_reachables = [@graph_hash[node][LAMBDA]].flatten
            lambda_reachables.each do |l_node|
              if !closure.include?(l_node)
                closure << l_node
                changed = true
              end
            end
          end
        end
      end

      return closure #.flatten
    end
    
    # returns true if the any of the closure's states are included in the set
    # of accept_states
    def accept_state_of(closure)
      closure.each do |set|
        if @accept_states.include?(set)
          return @accept_states[set] # change this to "true" if reverting to crappy system
        end
      end
      return false
    end
    
    # numbers sets and makes them the new keys (cleans up the graph_hash's keys)
    def beautify 
      clean_hash, clean_accept_states, pretty, new_labels = {}, {}, {}, {}
      
      # Number our closures.
      i = -1
      @graph_hash.each_pair do |key,subhash|
        pretty[key] = i+= 1 if pretty[key].nil?
        subhash.values.each {|subval| pretty[subval]=i+=1 if pretty[subval].nil?}
      end
      
      # Replace instances of old closure names with their new closure-numbers.
      @graph_hash.keys.each do |old_key|
        new_key = pretty[old_key]
        clean_hash[new_key] = Hash.new
        @graph_hash[old_key].each_pair do |subkey, subval| # subkey is edge label
          clean_hash[new_key][subkey] = pretty[subval]
        end
      end

      @accept_states.each_pair do |state, acc_type|
        clean_accept_states[pretty[state]] = acc_type
      end

      @labels.each_pair do |state, label|
        new_labels[pretty[state]] = @labels[state]
      end # Be sure to bring labels along.

      FiniteStateMachine.new({
        :graph_hash => clean_hash,
        :accept_states => clean_accept_states,
        :labels => new_labels
      })
    end
    
    def generate_initialize
      return "def initialize\n" +
        "#{" "*indent_width}#{lookup_code_string}\n" + # array of edge labels
        "#{" "*indent_width}#{label_code_string}\n" +  # hash mapping accept states to the type accepted by them
        dump_table(:state) + "\n" +
        dump_table(:action) + "\n" +
        dump_table(:lookup) + "\n" +
        "#{ind(1)}end"
    end

    # Module Dumping
    def generate_module(name = 'ScannerModule', indent_width = 2)
      expand_regex_edges
      return "module #{name}\n  def initialize\n" +
        "#{ind(1)}#{lookup_code_string}\n" + # array of edge labels
        "#{ind(1)}#{label_code_string}\n" +  # hash mapping accept states to the type accepted by them
        dump_table(:state) + "\n" +
        dump_table(:action) + "\n" +
        dump_table(:lookup) + "\n" +
        "#{ind(2)}super\n" +
        "#{ind(1)}end\nend"
    end

    # This horrific kluge ports the ruby dump_tables to Javascript. Sorta.
    # Smelly code, but output passes JSLint and is the path of least resistance.
    #
    # TODO: Write something that generates the tables as ruby objects, then
    # refactor these table dumping functions, using array.to_s
    def js_tables(name = 'ScannerModule', indent_width = 2)
      expand_regex_edges
      replacements = {
        '  # E'             => '//  E',   # Action table's label
        '  #  '             => '//   ',   # table-leading comments
        '# '                => ' // ',    # row-trailing descriptions
        '[['                => ' [',      # first row of table
        '@state_table = '   => 'SCANNER.state_table = [',
        '@action_table = '  => 'SCANNER.action_table = [',
        '@lookup_table = '  => 'SCANNER.lookup_table = [',
        '@lookup_codes'     => 'SCANNER.lookup_codes',
        '@label_codes'      => 'SCANNER.label_codes',
        '   ['              => '    [',   # linty js indent of 4
        ']] '               => ']];',     # semicolon ending tbales
        ':other'            => '"other"', # :other symbol.to_s
        '"=>'               => '" : '     # javascript hash notation
      }
      s = "var SCANNER = {};\n" +
        dump_table(:state, 0, 0) + "\n" +
        dump_table(:action, 0, 0) + "\n" +
        dump_table(:lookup, 0, 0) + "\n" +
        "#{ind(0)}#{lookup_code_string};\n" + # array of edge labels
        "#{ind(0)}#{label_code_string};\n"    # hash mapping accept states to the type accepted by them
      replacements.each_pair{|k,v| s.gsub!(k,v)}
      s
    end

    def generate_scanner(indent_width = 2)
      expand_regex_edges

      scanner_function =<<-'END_SCANNER'
    def scan(input)
      @token = ""
      @state = 0
      @buffered = false
      results = Array.new
      
      input.each_char do |ch|
        current_read = case ch # Map chars onto char-classes by editing case/when
          when /[a-zA-Z]/     then @label_codes["L"]
          when /[0-9]/        then @label_codes["D"]
          else @label_codes[ch] || @label_codes[:other]
        end
        if((@action_table[@state][current_read]==1) && (@state_table[@state][current_read] != -1))
          @buffered = false # action=MA (Machine-Accept) (=1). Append char to token.
          @token += ch unless ch[/\s/] && @label_codes[ch].nil? # Uncomment if recognizing some whitespace.
          @state=@state_table[@state][current_read]
        elsif((@state_table[@state][current_read]==-1) && (@action_table[@state][current_read]==2))
          @buffered = true # action=HR (Halt-Return) (=2). Accept current token.
          results.push [@lookup_codes[@lookup_table[@state][current_read]],@token]
          @state = 0
          @token = ""
        else # ? Hitting this block indicates action=ERR (ERROR) (=3)
          next
        end
        redo if(@buffered==true && current_read!=@label_codes[:other]) # repeat w/o advancing to next char
      end
      results
    end

    # Appends a newline to the file in case of its absence, to ensure
    # the presence of terminating whitespace. Convert Windows newlines
    # to UNIX style ones.
    def scan_file(filename = "test_file.txt")
      scan((File.open(filename, "r").read+"\n").gsub("\r\n","\n"))
    end
  END_SCANNER
      return "class Scanner\n" +
        "#{ind(1)}def initialize\n" +
          "#{ind(2)}#{lookup_code_string}\n" + # array of edge labels
          "#{ind(2)}#{label_code_string}\n" +  # hash mapping accept states to the type accepted by them
          dump_table(:state,  2,2) + "\n" + # Note: the 1s should be 2s, but dump_table's results seem
          dump_table(:action, 2,2) + "\n" + # to mysteriously have an extra leading two spaces. Can't
          dump_table(:lookup, 2,2) + "\n" + # for the life of me figure out how or why.
        "#{ind(1)}end\n\n" +
        scanner_function +
        "\nend"
    end

    # Module Dumping
    def dump_module(name, indent_width = 2)
      #return generate_module if filename == "" || filename.nil?
      filename = underscore(name)
      file = File.open("./modules/#{filename}.rb", "w")
      "Successfully wrote #{file.write(generate_module)} characters to #{filename}.rb"
    end
    
    def friendly_edge_labels
      # Convert whitespace line "\n" into strings describing their contents.
      get_edge_labels.collect do |label|
        (!label[/\s/].nil?) ? label.inspect[1..-2] : label
      end
    end
    
    def label_friendly(label)
      #puts "label: '#{label}' (#{label.class})"
      if label.class == Fixnum
        return label.to_s
      elsif label.class == Regexp
        return ('/' + label.to_s[7..-2] + '/') # replace each slash with 2 slashes.
      elsif label == LAMBDA || label.to_s == "LAMBDA" || label.to_s == "EPSILON" || label.to_s.empty? # http://stackoverflow.com/questions/9684807/how-can-one-insert-a-mathematical-greek-etc-symbol-in-dot-file
        '&#949;' # epsilon-lower is 949
      else
        return ((!label[/\s/].nil?) ? label.inspect[1..-2] : label)
      end
    end
    

    def ind(level, width=2)
      return " "*(level*width)
    end
    
    # converts a CamelCased name to k_and_r style for filename
    def underscore(name)
      s = name[0].downcase
      name[1,name.length].each_char do |ch|
        s += (ch.match(/[^A-Z]/) ? ch : "_"+ch.downcase)
      end
      return s
    end
    
    
    def lookup_codes
      (["!accept"] | @accept_states.values)
    end
    # Array unions: fuck yeah.
    def lookup_code_string
      "@lookup_codes = #{lookup_codes.to_s}"
    end
    
    def label_code_string
      h = {}
      get_edge_labels.each_with_index{|label,ii| h[label]=ii } # !!! DANGER
      "@label_codes = #{h.to_s[0..-2]}, :other=>#{get_edge_labels.length}\}"
    end

  # Graph Attachment
    def increment_node_labels(amount)
      new_hash, new_accepts = Hash.new, Hash.new

      @graph_hash.each_pair do |key,subhash|
        new_subhash = Hash.new
        subhash.each_pair do |subkey,value|
          if value.class == Fixnum
            new_subhash[subkey] = value+amount
          elsif value.class == Array
            new_subhash[subkey] = value.map {|n| n+amount}
          else
            raise "value (#{value}) is a #{value.class}!"
          end
          
        end
        new_hash[key+amount] = new_subhash
      end
      
      @accept_states.keys.each{|key| new_accepts[key+amount] = @accept_states[key]} 
      @graph_hash, @accept_states = new_hash, new_accepts
      @origin += amount
    end
    
    def get_node_count; get_node_names.size; end

    # considerations:
    #  do we need a flag for when we have to strip an attach point of being an accept state?
    def attach_graph(attach_point, fsm)
      node_count = get_node_count
      raise "#{attach_point} out of graph bounds."  if attach_point >= node_count
      raise "going to break everything by attaching to myself!" if fsm == self
      #dfa = fsm.subsetify #.subsetify
      dfa = fsm # Before, we were subsetifying
      dfa.increment_node_labels(node_count)
      #@graph_hash[attach_point] = {LAMBDA => dfa.origin} # THIS IS OUR CULPRIT!
      #puts "before #{@graph_hash[attach_point]}"
      
      #if (@graph_hash[attach_point]!=nil)
      #  lambdas = [@graph_hash[attach_point][LAMBDA]] || [] # this is an array!
      #  lambdas << dfa.origin
      #  lambdas = lambdas.flatten.find{|entry| !entry.nil?}
      #  @graph_hash[attach_point][LAMBDA] = lambdas
      #else
      #  @graph_hash[attach_point] = {LAMBDA => dfa.origin}
      #end

      # if attach point was on the graph w/o outgoing edges
      @graph_hash[attach_point] = Hash.new if @graph_hash[attach_point].nil?
        
      if @graph_hash[attach_point][LAMBDA].nil?
        @graph_hash[attach_point][LAMBDA] = [dfa.origin]
      else # attach point already has outgoing lambda edges
        @graph_hash[attach_point][LAMBDA] << dfa.origin
      end
      
      #@graph_hash[attach_point]["foo"] = lambdas
      #puts "after  #{@graph_hash[attach_point]}"
      #puts "@gh = #{graph_hash}\ndfah = #{dfa.graph_hash}"
      #puts "merged: #{@graph_hash.merge(dfa.graph_hash)}"
      @graph_hash.merge!(dfa.graph_hash)
      #@graph_hash.merge!({4=>{"L"=>21}})
      @accept_states.merge!(dfa.accept_states)
      #subsetify!
      get_node_count
    end

    # Dumps either a state table, an action table, or a lookup table
    # This function is kind of half-refactored with dump_module and needs cleaning
    # like the wizard needs food.
    def dump_table(type = :state, indent_width = 2, indent_level = 2)
      # edge_labels = friendly_edge_labels << "   Other" # I suspect this line is ruining the code.
      edge_labels = get_edge_labels << "   Other"
      node_names = get_node_names
      
      s = "#{ind(indent_level)}@#{type}_table = " +
          ((type == :action) ? "\n#{ind(indent_level+1)}\# ERROR = 0; MACHINE_ACCEPT = 1; HALT_RETURN = 2" : "") +
          "\n#{ind(indent_level+1)}#"
      edge_labels.each do |label|
        s += sprintf("%#{WIDTH+1}s", label_friendly(label))
      end
      s += "\n#{ind(indent_level+1)}"
      
      node_names.each_with_index do |node,ii|
        on_last_node = (ii == node_names.size-1)
        is_accept = !@accept_states[node].nil?
        s += ((ii==0) ? "[" : " ") + "["
        
        edge_labels.each_with_index do |edge,jj|
          on_last_edge = (jj == edge_labels.size-1)
          if(@graph_hash[node].nil?||
             @graph_hash[node][edge].nil?||@graph_hash[node][edge][0].nil?)
            sdest = "-1"
            adest = ((is_accept) ? HALT_RETURN.to_s : ERROR.to_s)
            if(!accept_states[node].nil?)
              ldest = ((is_accept) ? (lookup_codes.find_index(accept_states[node]).to_i).to_s : "0")
            else
              ldest = "0"
            end
          else
            sdest = graph_hash[node][edge].to_s
            adest = MACHINE_ACCEPT.to_s # MA if NON-ACCEPT state
            ldest = "0"
          end
          case type
            when :state
              s += sprintf("%#{WIDTH}s", sdest) + 
                ((!on_last_edge) ? "," \
                  : "]" + ((!on_last_node) ? "," \
                    : "]" ) + " \# #{node}#{(is_accept ? " ACCEPT":"")}\n#{ind(indent_level+1)}")
            when :action
              s += sprintf("%#{WIDTH}s", adest) + 
                (!on_last_edge ? "," \
                  : "]" + (!on_last_node ? "," \
                    : "]" ) + " \# #{node}#{(is_accept ? " ACCEPT" : "")}\n#{ind(indent_level+1)}")
            when :lookup
              s += sprintf("%#{WIDTH}s", ldest) + 
                (!on_last_edge ? "," \
                  : "]" + (!on_last_node ? "," \
                    : "]" ) + " \# #{node}#{(is_accept ? " #{@accept_states[node]}" : "")}\n#{ind(indent_level+1)}")
          end
        end
      end
      s.rstrip
    end

    # Clobbers the old accept type, if any was present.
    def add_accept_state(state, type)
      @accept_states[state] = type
    end

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
      # !!! TODO: Add code to handle (delete) orphaned nodes.
      self
    end

    def is_accept?(num)
      @accept_states.include?(num)
    end
  end
end