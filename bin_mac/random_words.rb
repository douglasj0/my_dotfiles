#!/usr/bin/ruby -wKU
# =================================================================================
# random_words
# 
# Created by Mark Hasse on 2008-04-11.
# =================================================================================
# Todo: Use a better random generator, for example random.org.
# =================================================================================

require "optparse"

# =================================================================================
class OptionsParser
  def self.parse(args)
    # set default values 
    $options             = Hash.new
    $options[:wordlist]  = '/usr/share/dict/words'
    $options[:passwords] = 10
    $options[:words]     = 2
    $options[:numbers]   = 2
    $options[:symbols]   = 1
    
    opts = OptionParser.new do |opts|
      opts.banner = "Usage #{File.basename($0)} [options]"
      
      opts.on("-l", "--word-list=FILE", "List of words") do |l|
        $options[:wordlist] = l
      end
      
      opts.on("-p", "--passwords=NUMBER", "Passwords to generate") do |p|
        $options[:passwords] = p.to_i
      end
      
      opts.on("-w", "--words=NUMBER", "Words to use in each password") do |w|
        $options[:words] = w.to_i
      end
      
      opts.on("-n", "--numbers=NUMBER", "Digits to use in the delimiter") do |n|
        $options[:numbers] = n.to_i
      end
      
      opts.on("-s", "--symbols=NUMBER", "Symbols to use in the delimiter") do |s|
        $options[:symbols] = s.to_i
      end
      
      opts.on_tail("-h", "-?", "--help", "Show this message") do
        puts opts
        exit
      end
    end # opts = OptionParser.new do |opts|
    
    begin
      opts.parse!(args)
    rescue Exception => e
      if e.to_s != 'exit'
        puts e.to_s, opts
        exit(1)
      else
        exit(0)
      end
    end 
  end # self.parse
  
end # OptionsParser

# =================================================================================
class Array
  def random
    self[rand(self.size)]
  end
end

# =================================================================================
def generate_filler
  chars = '~!@#$%^&*()_+-=[]{}\\|;:\'",<.>/?'
  numbs = '0123456789'

  delim = ''
  delim << sprintf( "%0#{$options[:numbers]}d", rand( 10**$options[:numbers] - 1 ) )
  $options[:symbols].times { delim << chars[rand(chars.length - 1)] }
  
  return(delim)
end         

# =================================================================================
def gen_passwd
  words    = IO.readlines($options[:wordlist])
  password = String.new
    
  $options[:words].times do |i|
    word = String.new
    while word.length == 0 || word.length > 6 do
      word = words.random.chomp
    end 
    password << word
    password << generate_filler   if i < ($options[:words] - 1)
  end
  
  return(password)
end        

# =================================================================================
# main
# =================================================================================
OptionsParser.parse(ARGV)

$options[:passwords].times do
  puts gen_passwd
end
