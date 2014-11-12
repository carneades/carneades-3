define [
  'root'
], (cn) ->
  mixOf = (base, mixins...) ->
    class Mixed extends base
      for mixin in mixins by -1 #earlier mixins override later ones
        for name, method of mixin::
          Mixed::[name] = method
      Mixed


  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  del = (obj, key) ->
    val =  obj[key]
    delete obj[key]
    val

  readData = (data) ->
    bi = 0
    buffer = []
    while data[bi]
      buffer.push data[bi]
      bi = bi + 1
    return buffer.join ''


  carneades = cn.carneades
  carneades.mixOf = mixOf
  carneades.extend = extend
  carneades.readData = readData
  carneades.del = del
