WORKING_DIR = File.dirname(__FILE__)
require 'rubygems'
require 'rb-inotify'
n = INotify::Notifier.new
n.watch("#{WORKING_DIR}/", :all_events) { |event|
  puts `#{WORKING_DIR}/build` if event.name == "report.tex" && event.flags.include?(:modify)
} 
n.run
