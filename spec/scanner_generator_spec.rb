require 'scanner_generator'

describe ScannerGenerator::FiniteStateMachine do
  it "generates graphs" do
    # obj.should eql(val)
  end

  it "has no epsilons/lambdas in DFAs" do
  end

  it "replaces edges successfully" do

  end

  it "handles edge cases" do
  end
end

# Radar's example test from Foodie:
#it "anything else is delicious" do
  #Foodie::Food.portray("Not Broccoli").should eql("Delicious!")
#end

=begin
# The below tests verify Thompson Construction (conversion from regular
# expressions to NFAs). The tests made output for human eyes.

# TODO: Verify the tests are all still satisfied correctly, then hard-code
# graphs and tables satisfying ".should eql()" invocation via RSpec.

# TODO: Track down the other tests.

# Non-RSpec test code follows.
#!/usr/bin/ruby
require '../../../cct/app/models/finite_state_machine.rb'
require "awesome_print"

def replace_edge_test
  fsa = FiniteStateMachine.new({
    :accept_states => {2=>'accm2', 3=>'accm3'},
    :graph_hash    => {
      0 => {"LAMBDA"=>[1]},
      1 => {"M2" => 2,
            "M3" => 3},
      2 => {"a" => 4 },
      4 => {"b" => 3}
    }
  })
  fsa2 = fsa.clone
  alter = FiniteStateMachine.new({
    :accept_states => {3=>'alt_end'},
    :graph_hash    => {
      0 => {"LAMBDA" => [1,4]},
      1 => {"M1" => 2},
      2 => {"LAMBDA" => 3},
      4 => {"M2" => 5},
      5 => {"LAMBDA" => 3}
    }
  })
  alt = FiniteStateMachine.new({
    :accept_states => {2 => 'end'},
    :graph_hash => {
      0 => {'a' => [1]},
      1 => {'b' => [2]}
    }
  })
  fsa.draw_graph("before")
  fsa.lambda_replace_edge(1,"M3",3, alt.clone)
  fsa.draw_graph("lambda-after")
  fsa2.replace_edge(1,"M3",3, alt.clone)
  fsa2.draw_graph("after")  
  
  #malt = alt.clone
  #malt.draw_graph('alt-before-imp-attach')
  #malt.imp_attach_graph(2,alt)
  #malt.imp_attach_graph(2,alter)
  #malt.imp_attach_graph(7,alt)
  #malt.draw_graph('alt-after-imp-attach')
end

def prepend_test
  fsa = FiniteStateMachine.new({
    :accept_states => {1=>'accept'},
    :graph_hash    => {0=>{"fuck"=>1}}
  })
  fsa.prepend_graph(fsa.kleene_machine)
  fsa.prepend_graph(fsa.cat_machine('LAMBDA'))
  fsa.draw_graph('prepend-test')
end



# Interesting notes:
#   (a|b|ab)* can read aab with more than one parse tree.

def re2nfa_test
  fsa = FiniteStateMachine.new({
    :accept_states => {1=>'accept'},
    :graph_hash    => {0=>{"LAMBDA"=>1}}}
  )
  example = FiniteStateMachine.new({
    :accept_states => {8=>"end"},
    :graph_hash => {
      0 => {"LAMBDA" => [1,3]},
      1 => {"LAMBDA" => [4,6]},
      2 => {"LAMBDA" => [1,3]},
      3 => {"c" => 8},
      4 => {"a" => 5},
      5 => {"LAMBDA" => 2},
      6 => {"b" => 7},
      7 => {"LAMBDA" => 2}
    },
    :origin => 0
  })

  #s = '(a|b)*c|de*|f'
  s = 'a|b(d*|(e|f)*)'
  #s = 'a((b))'
  # the following strings breaks it, in some way
    # 'a*|b|cde**|k' causes lots of duplication
      # see examples/2012-03-01_22-47-54_-0800-nfa.png
      # shows a* OR b OR replace_me, followed by
      # b | c
      # followed by c
    # '(a|b)*'
  #s = '(a|b)*c'
  puts "rendering regex: #{s}"
  fsa = fsa.re2nfa(s)
  fsa.draw_graph("draw_re2nfa")
  puts 'bad nfa'
  ap fsa
  fsa.subsetify.draw_graph("draw_re2dfa")
  example.draw_graph("ex_nfa")
  puts 'good nfa'
  ap example
  example.subsetify.draw_graph("ex_dfa")
  fsa.graph_hash.each_pair do |k,v|
    if v != example.graph_hash[k]
      puts "#{v} != #{example.graph_hash[k]}"
    else
      puts "same"
    end
  end
end

def re2nfa_ends_in_or_test
  fsa = FiniteStateMachine.new({
    :graph_hash=>{0 => {"1" => 1}},
    :accept_states => {}
  })
  fsa = fsa.re2nfa("1|0")
  fsa.add_edge(5,LAMBDA,6)
  fsa.set_new_accept(5,nil)
  fsa.set_new_accept(6,"ok")
  fsa.draw_graph("end_test_nfa")
  fsa.subsetify!
  fsa.draw_graph("end_test_dfa")
end

re2nfa_test
#injection_test
#replace_edge_test
#prepend_test
#re2nfa_ends_in_or_test
  
=end