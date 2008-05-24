require 'rubygems'
require 'rake'

ERLC_FLAGS = "+debug_info -W2 -o ../ebin"

task :default do
  cd "elibs"
  sh "erlc #{ERLC_FLAGS} *.erl"
end

task :console do
  sh "erl +Bc +K true -smp enable -pz ./ebin/ -sname local_console_#{$$} -kernel start_boot_server true"
end

task :go do
  sh "erl +Bc +K true -smp enable -pz ./ebin/ -sname local_console_#{$$} -kernel start_boot_server true -run gandalf start"
end