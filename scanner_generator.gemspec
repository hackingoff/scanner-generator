# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "scanner_generator/version"

Gem::Specification.new do |s|
  s.name        = "scanner_generator"
  s.version     = ScannerGenerator::VERSION
  s.authors     = ["Hacking Off"]
  s.email       = ["source@hackingoff.com"]
  s.homepage    = "https://github.com/hackingoff/scanner-generator"
  s.summary     = %q{Parser generation and CFG analysis.}
  s.description = %q{Part of the compiler construction toolkit's guts.}

  s.rubyforge_project = "scanner_generator"

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]

  s.add_development_dependency "rspec", "~> 2.6"
  s.add_development_dependency "awesome_print"

  s.add_dependency "ruby-graphviz" # graph visualizations

  # specify any dependencies here; for example:
  # s.add_development_dependency "rspec"
  # s.add_runtime_dependency "rest-client"
end
