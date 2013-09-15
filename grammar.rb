require 'bundler/setup'
require 'pegrb'

module SimpleLisp

  ## 
  # "scope" object for the interpreter

  class Env
    def initialize(env, outer)
      @env, @outer = env, outer
    end

    def [](key)
      @env[key] || @outer[key]
    end

    def define(key, value)
      if self[key]
        raise StandardError, "#{key} is already defined."
      end
      @env[key] = value
    end

    def set!(key, value)
      @env[key] ? @env[key] = value : @outer.set!(key, value)
    end
  end

  ## ast nodes

  class Integer < Struct.new(:value)
    def eval(env)
      IntegerEvaled.new(value)
    end
  end
  
  class Boolean < Struct.new(:value)
    def eval(env)
      BooleanEvaled.new(value)
    end
  end
  
  class Symbol < Struct.new(:value)
    def eval(env)
      env[value]
    end
  end

  ##
  # To evaluate a list we first take care of any
  # special forms and then we worry about the non-special
  # forms. At the end of the day non-special forms just
  # become function calls.

  class List < Struct.new(:value)
    def eval(env)
      head, *tail = *value
      case head.value
      when :set!
        variable = tail[0].value
        value = tail[1].eval(env)
        env.set!(variable, value)
      when :define
        variable = tail[0].value
        value = tail[1].eval(env)
        env.define(variable, value)
      when :'define-macro'
        macro_name = tail[0].value
        macro_args = tail[1].value.map(&:value)
        if macro_args.length != macro_args.uniq.length
          raise StandardError, "Macro arguments must be unique."
        end
        env.define(macro_name, tail)
        binding.pry
      when :begin
        return_value = nil
        tail.each {|node| return_value = node.eval(env)}
        return_value
      when :quote
        tail.first
      when :if
        boolean = tail[0].eval(env)
        boolean.value ? tail[1].eval(env) : tail[2].eval(env)
      when :lambda
        variables = tail[0].value.map(&:value)
        if variables.length != variables.uniq.length
          raise StandardError, "Lambda parameters must be unique."
        end
        func = ->(*args) {
          if args.length != variables.length
            raise StandardError, "Wrong number of arguments. Expected #{variables.length} argument(s)."
          end
          variable_argument_pairs = variables.zip(args).flatten
          function_environment = Env.new(Hash[*variable_argument_pairs], env)
          tail[1].eval(function_environment)
        }
      when :let
        variable_value_pairs = tail[0].value.map do |pair|
          variable = pair[0].value
          value = pair[1].eval(env)
          [variable, value]
        end.flatten
        let_environment = Env.new(Hash[*variable_value_pairs], env)
        tail[1].eval(let_environment)
      else
        evaluated_arguments = tail.map {|node| node.eval(env)}
        head.eval(env).call(*evaluated_arguments)
      end
    end
  end

  # evaluated nodes
  class ListEvaled < Struct.new(:value); end
  class IntegerEvaled < Struct.new(:value); end
  class BooleanEvaled < Struct.new(:value); end

  @grammar = Grammar.rules do
    # basic punctuation
    lpar, rpar, ws = one_of('('), one_of(')'), one_of(' ', "\n", "\t", "\r").many

    # comment starts with ';;' and goes to end of line
    comment = (m(';;') > cut! > (!one_of("\n", "\r") > wildcard).many.any >
     one_of("\n", "\r")).ignore

    # integer 123, 01234, 41234324, etc.
    integer = one_of(/[0-9]/).many[:digits] >> ->(s) {
      [Integer.new(s[:digits].map(&:text).join.to_i)]
    }

    # boolean only #t or #f
    boolean = (m('#t') | m('#f'))[:boolean] >> ->(s) {
      [Boolean.new(s[:boolean].map(&:text).join == '#t')]
    }

    # symbol is any sequence of "valid characters"
    symbol = one_of(/[a-zA-Z\_\?\!\+\\\-\=\<\@\#\$\%\^\&\*]/).many[:characters] >> ->(s) {
      [Symbol.new(s[:characters].map(&:text).join.to_sym)]
    }

    # atom can be symbol, boolean, int
    atom = integer | boolean | symbol

    # list can be empty (nil) or non-empty (head tail*)
    empty = m('nil') >> ->(s) {
      [List.new([])]
    }
    nonempty = (lpar > cut! > ws.any > r(:expression)[:head] >
     (ws.ignore > r(:expression)).many.any[:tail] > rpar) >> ->(s) {
      [List.new(s[:head] + s[:tail])]
    }
    rule :list, empty | nonempty

    # lisp has quotes so lets do that too: 'expression
    rule :quote, (one_of("'") > cut! > r(:expression)[:expression]) >> ->(s) {
      [List.new([Symbol.new(:quote), s[:expression].first])]
    }

    # valid expressions in our language
    rule :expression, r(:list) | r(:quote) | atom | comment

    # start
    rule :start, r(:expression)[:expression] >> ->(s) {
      s[:expression].first
    }
  end

  def self.parse(string); @grammar.parse(string); end

  def self.initial_environment
    @initial_environment ||= Env.new({
      :+ => ->(*args) {IntegerEvaled.new(args.map(&:value).reduce(:+))},
      :- => ->(*args) {IntegerEvaled.new(args.map(&:value).reduce(:-))},
      :/ => ->(*args) {IntegerEvaled.new(args.map(&:value).reduce(:/))},
      :* => ->(*args) {IntegerEvaled.new(args.map(&:value).reduce(:*))},
      :'=' => ->(*args) {head, *rest = *args; BooleanEvaled.new(rest.all? {|x| x == head})},
      :and => ->(*args) {BooleanEvaled.new(args.all? {|x| x.value})},
      :or => ->(*args) {BooleanEvaled.new(args.any? {|x| x.value})},
      :not => ->(boolean) {BooleanEvaled.new(!boolean.value)},
      :cons => ->(head, tail) {ListEvaled.new([head] + tail.value)},
      :car => ->(list) {list.first},
      :cdr => ->(list) {ListEvaled.new(list.value[1..-1])},
      :< => ->(*args) {
        first, second = args.first.value - 2, args.first.value - 1
        args.each do |i|
          if first >= second
            return BooleanEvaled.new(false)
          end
          first, second = second, i.value
        end
        BooleanEvaled.new(first < second)
      }
    }, {})
  end

  def self.eval(string)
    @grammar.parse(string).eval(initial_environment)
  end

end
