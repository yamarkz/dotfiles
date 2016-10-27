require 'irb/completion'
require 'pp'
require 'rubygems'
require 'wirble'
require 'awesome_print'

# HISTORY
IRB.conf[:SAVE_HISTORY] = 100000

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:AUTO_INDENT_MODE] = false

Wirble.init
Wirble.colorize
AwsomePrint.irb!
