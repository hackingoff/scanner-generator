# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "scanner_generator/version"

Gem::Specification.new do |s|
  s.name        = "scanner_generator"
  s.version     = ScannerGenerator::VERSION
  s.authors     = ["Hacking Off"]
  s.email       = ["source@hackingoff.com"]
  s.homepage    = "https://github.com/hackingoff/context-free-grammar"
  s.summary     = %q{TODO: Write a gem summary}
  s.description = %q{TODO: Write a gem description}

  s.rubyforge_project = "scanner_generator"

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]

  s.add_development_dependency "rspec", "~> 2.6"
  s.add_development_dependency "awesome_print"

  s.add_dependency "ruby-graphviz" # Needed for Parser graph visualizations.

  # specify any dependencies here; for example:
  # s.add_development_dependency "rspec"
  # s.add_runtime_dependency "rest-client"
end
