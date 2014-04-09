class DS
  constructor: (@length) ->
    @data = new Array @length
  size: -> throw new Error('I am an abstract class!')
  push: (s) -> throw new Error('I am an abstract class!')
  pop: -> throw new Error('I am an abstract class!')
  peek: -> throw new Error('I am an abstract class!')

class Stack extends DS
  size: -> return @data.length
  push: (s) -> @data.push s
  pop: ->
    s = @data[@data.length - 1]
    @data = @data.slice 0, @data.length - 1
    return s
  peek: -> return @data[@data.length - 1]

class BCStack extends Stack
  size: -> return @data.length
  push: (s) -> super.push(s)
  pop: -> return super.pop()
  peek: -> return super.peek()

class Queue extends DS
  constructor: (length) ->
    super(length)
    @ptr = -1
  size: -> return @length
  push: (s) ->
    @ptr = @ptr + 1
    @ptr = @ptr % @length
    @data[@ptr] = s
  pop: () ->
    s = @data[@ptr]
    @ptr = if @ptr is 0 or @ptr is -1 then @ptr = @length else @ptr - 1
    return s
  peek: () -> return @data[@ptr]
  asArray: ->
    arr = []
    i = @length
    p = @ptr
    unless i is 0
      do
        arr.push @data[p]
        p = p + 1
        p = p % @length
        i = i - 1
    return arr

console.log "STACK"
s = new BCStack()
console.log 'Size: ' + s.size()
s.push 1
console.log 'Size: ' + s.size()
e = s.peek()
console.log 'Element: ' + e
console.log 'Size: ' + s.size()
s.pop()
console.log 'Size: ' + s.size()

console.log "QUEUE MIX"
q = new Queue(7)
console.log 'Size: ' + q.size()
q.push 1
console.log "check element at #{q.ptr}: " + q.peek()
q.push 3
console.log "check element at #{q.ptr}: " + q.peek()
q.pop()
console.log "check element at #{q.ptr}: " + q.peek()
q.push 5
console.log "check element at #{q.ptr}: " + q.peek()

console.log "QUEUE pop()"
q = new Queue(2)
q.pop()
console.log "check element at #{q.ptr}: " + q.peek()
q.pop()
console.log "check element at #{q.ptr}: " + q.peek()
q.pop()
console.log "check element at #{q.ptr}: " + q.peek()
q.pop()
console.log "check element at #{q.ptr}: " + q.peek()

console.log "QUEUE push()"
q = new Queue(2)
q.push 1
console.log "check element at #{q.ptr}: " + q.peek()
q.push 2
console.log "check element at #{q.ptr}: " + q.peek()
q.push 3
console.log "check element at #{q.ptr}: " + q.peek()
q.push 4
console.log "check element at #{q.ptr}: " + q.peek()
