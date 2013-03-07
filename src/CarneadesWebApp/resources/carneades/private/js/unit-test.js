var CLOSURE_NO_DEPS = true;
var COMPILED = false;
var goog = goog || {};
goog.global = this;
goog.DEBUG = true;
goog.LOCALE = "en";
goog.provide = function(name) {
  if(!COMPILED) {
    if(goog.isProvided_(name)) {
      throw Error('Namespace "' + name + '" already declared.');
    }
    delete goog.implicitNamespaces_[name];
    var namespace = name;
    while(namespace = namespace.substring(0, namespace.lastIndexOf("."))) {
      if(goog.getObjectByName(namespace)) {
        break
      }
      goog.implicitNamespaces_[namespace] = true
    }
  }
  goog.exportPath_(name)
};
goog.setTestOnly = function(opt_message) {
  if(COMPILED && !goog.DEBUG) {
    opt_message = opt_message || "";
    throw Error("Importing test-only code into non-debug environment" + opt_message ? ": " + opt_message : ".");
  }
};
if(!COMPILED) {
  goog.isProvided_ = function(name) {
    return!goog.implicitNamespaces_[name] && !!goog.getObjectByName(name)
  };
  goog.implicitNamespaces_ = {}
}
goog.exportPath_ = function(name, opt_object, opt_objectToExportTo) {
  var parts = name.split(".");
  var cur = opt_objectToExportTo || goog.global;
  if(!(parts[0] in cur) && cur.execScript) {
    cur.execScript("var " + parts[0])
  }
  for(var part;parts.length && (part = parts.shift());) {
    if(!parts.length && goog.isDef(opt_object)) {
      cur[part] = opt_object
    }else {
      if(cur[part]) {
        cur = cur[part]
      }else {
        cur = cur[part] = {}
      }
    }
  }
};
goog.getObjectByName = function(name, opt_obj) {
  var parts = name.split(".");
  var cur = opt_obj || goog.global;
  for(var part;part = parts.shift();) {
    if(goog.isDefAndNotNull(cur[part])) {
      cur = cur[part]
    }else {
      return null
    }
  }
  return cur
};
goog.globalize = function(obj, opt_global) {
  var global = opt_global || goog.global;
  for(var x in obj) {
    global[x] = obj[x]
  }
};
goog.addDependency = function(relPath, provides, requires) {
  if(!COMPILED) {
    var provide, require;
    var path = relPath.replace(/\\/g, "/");
    var deps = goog.dependencies_;
    for(var i = 0;provide = provides[i];i++) {
      deps.nameToPath[provide] = path;
      if(!(path in deps.pathToNames)) {
        deps.pathToNames[path] = {}
      }
      deps.pathToNames[path][provide] = true
    }
    for(var j = 0;require = requires[j];j++) {
      if(!(path in deps.requires)) {
        deps.requires[path] = {}
      }
      deps.requires[path][require] = true
    }
  }
};
goog.ENABLE_DEBUG_LOADER = true;
goog.require = function(name) {
  if(!COMPILED) {
    if(goog.isProvided_(name)) {
      return
    }
    if(goog.ENABLE_DEBUG_LOADER) {
      var path = goog.getPathFromDeps_(name);
      if(path) {
        goog.included_[path] = true;
        goog.writeScripts_();
        return
      }
    }
    var errorMessage = "goog.require could not find: " + name;
    if(goog.global.console) {
      goog.global.console["error"](errorMessage)
    }
    throw Error(errorMessage);
  }
};
goog.basePath = "";
goog.global.CLOSURE_BASE_PATH;
goog.global.CLOSURE_NO_DEPS;
goog.global.CLOSURE_IMPORT_SCRIPT;
goog.nullFunction = function() {
};
goog.identityFunction = function(var_args) {
  return arguments[0]
};
goog.abstractMethod = function() {
  throw Error("unimplemented abstract method");
};
goog.addSingletonGetter = function(ctor) {
  ctor.getInstance = function() {
    return ctor.instance_ || (ctor.instance_ = new ctor)
  }
};
if(!COMPILED && goog.ENABLE_DEBUG_LOADER) {
  goog.included_ = {};
  goog.dependencies_ = {pathToNames:{}, nameToPath:{}, requires:{}, visited:{}, written:{}};
  goog.inHtmlDocument_ = function() {
    var doc = goog.global.document;
    return typeof doc != "undefined" && "write" in doc
  };
  goog.findBasePath_ = function() {
    if(goog.global.CLOSURE_BASE_PATH) {
      goog.basePath = goog.global.CLOSURE_BASE_PATH;
      return
    }else {
      if(!goog.inHtmlDocument_()) {
        return
      }
    }
    var doc = goog.global.document;
    var scripts = doc.getElementsByTagName("script");
    for(var i = scripts.length - 1;i >= 0;--i) {
      var src = scripts[i].src;
      var qmark = src.lastIndexOf("?");
      var l = qmark == -1 ? src.length : qmark;
      if(src.substr(l - 7, 7) == "base.js") {
        goog.basePath = src.substr(0, l - 7);
        return
      }
    }
  };
  goog.importScript_ = function(src) {
    var importScript = goog.global.CLOSURE_IMPORT_SCRIPT || goog.writeScriptTag_;
    if(!goog.dependencies_.written[src] && importScript(src)) {
      goog.dependencies_.written[src] = true
    }
  };
  goog.writeScriptTag_ = function(src) {
    if(goog.inHtmlDocument_()) {
      var doc = goog.global.document;
      doc.write('<script type="text/javascript" src="' + src + '"></' + "script>");
      return true
    }else {
      return false
    }
  };
  goog.writeScripts_ = function() {
    var scripts = [];
    var seenScript = {};
    var deps = goog.dependencies_;
    function visitNode(path) {
      if(path in deps.written) {
        return
      }
      if(path in deps.visited) {
        if(!(path in seenScript)) {
          seenScript[path] = true;
          scripts.push(path)
        }
        return
      }
      deps.visited[path] = true;
      if(path in deps.requires) {
        for(var requireName in deps.requires[path]) {
          if(!goog.isProvided_(requireName)) {
            if(requireName in deps.nameToPath) {
              visitNode(deps.nameToPath[requireName])
            }else {
              throw Error("Undefined nameToPath for " + requireName);
            }
          }
        }
      }
      if(!(path in seenScript)) {
        seenScript[path] = true;
        scripts.push(path)
      }
    }
    for(var path in goog.included_) {
      if(!deps.written[path]) {
        visitNode(path)
      }
    }
    for(var i = 0;i < scripts.length;i++) {
      if(scripts[i]) {
        goog.importScript_(goog.basePath + scripts[i])
      }else {
        throw Error("Undefined script input");
      }
    }
  };
  goog.getPathFromDeps_ = function(rule) {
    if(rule in goog.dependencies_.nameToPath) {
      return goog.dependencies_.nameToPath[rule]
    }else {
      return null
    }
  };
  goog.findBasePath_();
  if(!goog.global.CLOSURE_NO_DEPS) {
    goog.importScript_(goog.basePath + "deps.js")
  }
}
goog.typeOf = function(value) {
  var s = typeof value;
  if(s == "object") {
    if(value) {
      if(value instanceof Array) {
        return"array"
      }else {
        if(value instanceof Object) {
          return s
        }
      }
      var className = Object.prototype.toString.call(value);
      if(className == "[object Window]") {
        return"object"
      }
      if(className == "[object Array]" || typeof value.length == "number" && typeof value.splice != "undefined" && typeof value.propertyIsEnumerable != "undefined" && !value.propertyIsEnumerable("splice")) {
        return"array"
      }
      if(className == "[object Function]" || typeof value.call != "undefined" && typeof value.propertyIsEnumerable != "undefined" && !value.propertyIsEnumerable("call")) {
        return"function"
      }
    }else {
      return"null"
    }
  }else {
    if(s == "function" && typeof value.call == "undefined") {
      return"object"
    }
  }
  return s
};
goog.propertyIsEnumerableCustom_ = function(object, propName) {
  if(propName in object) {
    for(var key in object) {
      if(key == propName && Object.prototype.hasOwnProperty.call(object, propName)) {
        return true
      }
    }
  }
  return false
};
goog.propertyIsEnumerable_ = function(object, propName) {
  if(object instanceof Object) {
    return Object.prototype.propertyIsEnumerable.call(object, propName)
  }else {
    return goog.propertyIsEnumerableCustom_(object, propName)
  }
};
goog.isDef = function(val) {
  return val !== undefined
};
goog.isNull = function(val) {
  return val === null
};
goog.isDefAndNotNull = function(val) {
  return val != null
};
goog.isArray = function(val) {
  return goog.typeOf(val) == "array"
};
goog.isArrayLike = function(val) {
  var type = goog.typeOf(val);
  return type == "array" || type == "object" && typeof val.length == "number"
};
goog.isDateLike = function(val) {
  return goog.isObject(val) && typeof val.getFullYear == "function"
};
goog.isString = function(val) {
  return typeof val == "string"
};
goog.isBoolean = function(val) {
  return typeof val == "boolean"
};
goog.isNumber = function(val) {
  return typeof val == "number"
};
goog.isFunction = function(val) {
  return goog.typeOf(val) == "function"
};
goog.isObject = function(val) {
  var type = goog.typeOf(val);
  return type == "object" || type == "array" || type == "function"
};
goog.getUid = function(obj) {
  return obj[goog.UID_PROPERTY_] || (obj[goog.UID_PROPERTY_] = ++goog.uidCounter_)
};
goog.removeUid = function(obj) {
  if("removeAttribute" in obj) {
    obj.removeAttribute(goog.UID_PROPERTY_)
  }
  try {
    delete obj[goog.UID_PROPERTY_]
  }catch(ex) {
  }
};
goog.UID_PROPERTY_ = "closure_uid_" + Math.floor(Math.random() * 2147483648).toString(36);
goog.uidCounter_ = 0;
goog.getHashCode = goog.getUid;
goog.removeHashCode = goog.removeUid;
goog.cloneObject = function(obj) {
  var type = goog.typeOf(obj);
  if(type == "object" || type == "array") {
    if(obj.clone) {
      return obj.clone()
    }
    var clone = type == "array" ? [] : {};
    for(var key in obj) {
      clone[key] = goog.cloneObject(obj[key])
    }
    return clone
  }
  return obj
};
Object.prototype.clone;
goog.bindNative_ = function(fn, selfObj, var_args) {
  return fn.call.apply(fn.bind, arguments)
};
goog.bindJs_ = function(fn, selfObj, var_args) {
  if(!fn) {
    throw new Error;
  }
  if(arguments.length > 2) {
    var boundArgs = Array.prototype.slice.call(arguments, 2);
    return function() {
      var newArgs = Array.prototype.slice.call(arguments);
      Array.prototype.unshift.apply(newArgs, boundArgs);
      return fn.apply(selfObj, newArgs)
    }
  }else {
    return function() {
      return fn.apply(selfObj, arguments)
    }
  }
};
goog.bind = function(fn, selfObj, var_args) {
  if(Function.prototype.bind && Function.prototype.bind.toString().indexOf("native code") != -1) {
    goog.bind = goog.bindNative_
  }else {
    goog.bind = goog.bindJs_
  }
  return goog.bind.apply(null, arguments)
};
goog.partial = function(fn, var_args) {
  var args = Array.prototype.slice.call(arguments, 1);
  return function() {
    var newArgs = Array.prototype.slice.call(arguments);
    newArgs.unshift.apply(newArgs, args);
    return fn.apply(this, newArgs)
  }
};
goog.mixin = function(target, source) {
  for(var x in source) {
    target[x] = source[x]
  }
};
goog.now = Date.now || function() {
  return+new Date
};
goog.globalEval = function(script) {
  if(goog.global.execScript) {
    goog.global.execScript(script, "JavaScript")
  }else {
    if(goog.global.eval) {
      if(goog.evalWorksForGlobals_ == null) {
        goog.global.eval("var _et_ = 1;");
        if(typeof goog.global["_et_"] != "undefined") {
          delete goog.global["_et_"];
          goog.evalWorksForGlobals_ = true
        }else {
          goog.evalWorksForGlobals_ = false
        }
      }
      if(goog.evalWorksForGlobals_) {
        goog.global.eval(script)
      }else {
        var doc = goog.global.document;
        var scriptElt = doc.createElement("script");
        scriptElt.type = "text/javascript";
        scriptElt.defer = false;
        scriptElt.appendChild(doc.createTextNode(script));
        doc.body.appendChild(scriptElt);
        doc.body.removeChild(scriptElt)
      }
    }else {
      throw Error("goog.globalEval not available");
    }
  }
};
goog.evalWorksForGlobals_ = null;
goog.cssNameMapping_;
goog.cssNameMappingStyle_;
goog.getCssName = function(className, opt_modifier) {
  var getMapping = function(cssName) {
    return goog.cssNameMapping_[cssName] || cssName
  };
  var renameByParts = function(cssName) {
    var parts = cssName.split("-");
    var mapped = [];
    for(var i = 0;i < parts.length;i++) {
      mapped.push(getMapping(parts[i]))
    }
    return mapped.join("-")
  };
  var rename;
  if(goog.cssNameMapping_) {
    rename = goog.cssNameMappingStyle_ == "BY_WHOLE" ? getMapping : renameByParts
  }else {
    rename = function(a) {
      return a
    }
  }
  if(opt_modifier) {
    return className + "-" + rename(opt_modifier)
  }else {
    return rename(className)
  }
};
goog.setCssNameMapping = function(mapping, opt_style) {
  goog.cssNameMapping_ = mapping;
  goog.cssNameMappingStyle_ = opt_style
};
goog.global.CLOSURE_CSS_NAME_MAPPING;
if(!COMPILED && goog.global.CLOSURE_CSS_NAME_MAPPING) {
  goog.cssNameMapping_ = goog.global.CLOSURE_CSS_NAME_MAPPING
}
goog.getMsg = function(str, opt_values) {
  var values = opt_values || {};
  for(var key in values) {
    var value = ("" + values[key]).replace(/\$/g, "$$$$");
    str = str.replace(new RegExp("\\{\\$" + key + "\\}", "gi"), value)
  }
  return str
};
goog.exportSymbol = function(publicPath, object, opt_objectToExportTo) {
  goog.exportPath_(publicPath, object, opt_objectToExportTo)
};
goog.exportProperty = function(object, publicName, symbol) {
  object[publicName] = symbol
};
goog.inherits = function(childCtor, parentCtor) {
  function tempCtor() {
  }
  tempCtor.prototype = parentCtor.prototype;
  childCtor.superClass_ = parentCtor.prototype;
  childCtor.prototype = new tempCtor;
  childCtor.prototype.constructor = childCtor
};
goog.base = function(me, opt_methodName, var_args) {
  var caller = arguments.callee.caller;
  if(caller.superClass_) {
    return caller.superClass_.constructor.apply(me, Array.prototype.slice.call(arguments, 1))
  }
  var args = Array.prototype.slice.call(arguments, 2);
  var foundCaller = false;
  for(var ctor = me.constructor;ctor;ctor = ctor.superClass_ && ctor.superClass_.constructor) {
    if(ctor.prototype[opt_methodName] === caller) {
      foundCaller = true
    }else {
      if(foundCaller) {
        return ctor.prototype[opt_methodName].apply(me, args)
      }
    }
  }
  if(me[opt_methodName] === caller) {
    return me.constructor.prototype[opt_methodName].apply(me, args)
  }else {
    throw Error("goog.base called from a method of one name " + "to a method of a different name");
  }
};
goog.scope = function(fn) {
  fn.call(goog.global)
};
goog.provide("goog.debug.Error");
goog.debug.Error = function(opt_msg) {
  this.stack = (new Error).stack || "";
  if(opt_msg) {
    this.message = String(opt_msg)
  }
};
goog.inherits(goog.debug.Error, Error);
goog.debug.Error.prototype.name = "CustomError";
goog.provide("goog.string");
goog.provide("goog.string.Unicode");
goog.string.Unicode = {NBSP:"\u00a0"};
goog.string.startsWith = function(str, prefix) {
  return str.lastIndexOf(prefix, 0) == 0
};
goog.string.endsWith = function(str, suffix) {
  var l = str.length - suffix.length;
  return l >= 0 && str.indexOf(suffix, l) == l
};
goog.string.caseInsensitiveStartsWith = function(str, prefix) {
  return goog.string.caseInsensitiveCompare(prefix, str.substr(0, prefix.length)) == 0
};
goog.string.caseInsensitiveEndsWith = function(str, suffix) {
  return goog.string.caseInsensitiveCompare(suffix, str.substr(str.length - suffix.length, suffix.length)) == 0
};
goog.string.subs = function(str, var_args) {
  for(var i = 1;i < arguments.length;i++) {
    var replacement = String(arguments[i]).replace(/\$/g, "$$$$");
    str = str.replace(/\%s/, replacement)
  }
  return str
};
goog.string.collapseWhitespace = function(str) {
  return str.replace(/[\s\xa0]+/g, " ").replace(/^\s+|\s+$/g, "")
};
goog.string.isEmpty = function(str) {
  return/^[\s\xa0]*$/.test(str)
};
goog.string.isEmptySafe = function(str) {
  return goog.string.isEmpty(goog.string.makeSafe(str))
};
goog.string.isBreakingWhitespace = function(str) {
  return!/[^\t\n\r ]/.test(str)
};
goog.string.isAlpha = function(str) {
  return!/[^a-zA-Z]/.test(str)
};
goog.string.isNumeric = function(str) {
  return!/[^0-9]/.test(str)
};
goog.string.isAlphaNumeric = function(str) {
  return!/[^a-zA-Z0-9]/.test(str)
};
goog.string.isSpace = function(ch) {
  return ch == " "
};
goog.string.isUnicodeChar = function(ch) {
  return ch.length == 1 && ch >= " " && ch <= "~" || ch >= "\u0080" && ch <= "\ufffd"
};
goog.string.stripNewlines = function(str) {
  return str.replace(/(\r\n|\r|\n)+/g, " ")
};
goog.string.canonicalizeNewlines = function(str) {
  return str.replace(/(\r\n|\r|\n)/g, "\n")
};
goog.string.normalizeWhitespace = function(str) {
  return str.replace(/\xa0|\s/g, " ")
};
goog.string.normalizeSpaces = function(str) {
  return str.replace(/\xa0|[ \t]+/g, " ")
};
goog.string.collapseBreakingSpaces = function(str) {
  return str.replace(/[\t\r\n ]+/g, " ").replace(/^[\t\r\n ]+|[\t\r\n ]+$/g, "")
};
goog.string.trim = function(str) {
  return str.replace(/^[\s\xa0]+|[\s\xa0]+$/g, "")
};
goog.string.trimLeft = function(str) {
  return str.replace(/^[\s\xa0]+/, "")
};
goog.string.trimRight = function(str) {
  return str.replace(/[\s\xa0]+$/, "")
};
goog.string.caseInsensitiveCompare = function(str1, str2) {
  var test1 = String(str1).toLowerCase();
  var test2 = String(str2).toLowerCase();
  if(test1 < test2) {
    return-1
  }else {
    if(test1 == test2) {
      return 0
    }else {
      return 1
    }
  }
};
goog.string.numerateCompareRegExp_ = /(\.\d+)|(\d+)|(\D+)/g;
goog.string.numerateCompare = function(str1, str2) {
  if(str1 == str2) {
    return 0
  }
  if(!str1) {
    return-1
  }
  if(!str2) {
    return 1
  }
  var tokens1 = str1.toLowerCase().match(goog.string.numerateCompareRegExp_);
  var tokens2 = str2.toLowerCase().match(goog.string.numerateCompareRegExp_);
  var count = Math.min(tokens1.length, tokens2.length);
  for(var i = 0;i < count;i++) {
    var a = tokens1[i];
    var b = tokens2[i];
    if(a != b) {
      var num1 = parseInt(a, 10);
      if(!isNaN(num1)) {
        var num2 = parseInt(b, 10);
        if(!isNaN(num2) && num1 - num2) {
          return num1 - num2
        }
      }
      return a < b ? -1 : 1
    }
  }
  if(tokens1.length != tokens2.length) {
    return tokens1.length - tokens2.length
  }
  return str1 < str2 ? -1 : 1
};
goog.string.encodeUriRegExp_ = /^[a-zA-Z0-9\-_.!~*'()]*$/;
goog.string.urlEncode = function(str) {
  str = String(str);
  if(!goog.string.encodeUriRegExp_.test(str)) {
    return encodeURIComponent(str)
  }
  return str
};
goog.string.urlDecode = function(str) {
  return decodeURIComponent(str.replace(/\+/g, " "))
};
goog.string.newLineToBr = function(str, opt_xml) {
  return str.replace(/(\r\n|\r|\n)/g, opt_xml ? "<br />" : "<br>")
};
goog.string.htmlEscape = function(str, opt_isLikelyToContainHtmlChars) {
  if(opt_isLikelyToContainHtmlChars) {
    return str.replace(goog.string.amperRe_, "&amp;").replace(goog.string.ltRe_, "&lt;").replace(goog.string.gtRe_, "&gt;").replace(goog.string.quotRe_, "&quot;")
  }else {
    if(!goog.string.allRe_.test(str)) {
      return str
    }
    if(str.indexOf("&") != -1) {
      str = str.replace(goog.string.amperRe_, "&amp;")
    }
    if(str.indexOf("<") != -1) {
      str = str.replace(goog.string.ltRe_, "&lt;")
    }
    if(str.indexOf(">") != -1) {
      str = str.replace(goog.string.gtRe_, "&gt;")
    }
    if(str.indexOf('"') != -1) {
      str = str.replace(goog.string.quotRe_, "&quot;")
    }
    return str
  }
};
goog.string.amperRe_ = /&/g;
goog.string.ltRe_ = /</g;
goog.string.gtRe_ = />/g;
goog.string.quotRe_ = /\"/g;
goog.string.allRe_ = /[&<>\"]/;
goog.string.unescapeEntities = function(str) {
  if(goog.string.contains(str, "&")) {
    if("document" in goog.global) {
      return goog.string.unescapeEntitiesUsingDom_(str)
    }else {
      return goog.string.unescapePureXmlEntities_(str)
    }
  }
  return str
};
goog.string.unescapeEntitiesUsingDom_ = function(str) {
  var seen = {"&amp;":"&", "&lt;":"<", "&gt;":">", "&quot;":'"'};
  var div = document.createElement("div");
  return str.replace(goog.string.HTML_ENTITY_PATTERN_, function(s, entity) {
    var value = seen[s];
    if(value) {
      return value
    }
    if(entity.charAt(0) == "#") {
      var n = Number("0" + entity.substr(1));
      if(!isNaN(n)) {
        value = String.fromCharCode(n)
      }
    }
    if(!value) {
      div.innerHTML = s + " ";
      value = div.firstChild.nodeValue.slice(0, -1)
    }
    return seen[s] = value
  })
};
goog.string.unescapePureXmlEntities_ = function(str) {
  return str.replace(/&([^;]+);/g, function(s, entity) {
    switch(entity) {
      case "amp":
        return"&";
      case "lt":
        return"<";
      case "gt":
        return">";
      case "quot":
        return'"';
      default:
        if(entity.charAt(0) == "#") {
          var n = Number("0" + entity.substr(1));
          if(!isNaN(n)) {
            return String.fromCharCode(n)
          }
        }
        return s
    }
  })
};
goog.string.HTML_ENTITY_PATTERN_ = /&([^;\s<&]+);?/g;
goog.string.whitespaceEscape = function(str, opt_xml) {
  return goog.string.newLineToBr(str.replace(/  /g, " &#160;"), opt_xml)
};
goog.string.stripQuotes = function(str, quoteChars) {
  var length = quoteChars.length;
  for(var i = 0;i < length;i++) {
    var quoteChar = length == 1 ? quoteChars : quoteChars.charAt(i);
    if(str.charAt(0) == quoteChar && str.charAt(str.length - 1) == quoteChar) {
      return str.substring(1, str.length - 1)
    }
  }
  return str
};
goog.string.truncate = function(str, chars, opt_protectEscapedCharacters) {
  if(opt_protectEscapedCharacters) {
    str = goog.string.unescapeEntities(str)
  }
  if(str.length > chars) {
    str = str.substring(0, chars - 3) + "..."
  }
  if(opt_protectEscapedCharacters) {
    str = goog.string.htmlEscape(str)
  }
  return str
};
goog.string.truncateMiddle = function(str, chars, opt_protectEscapedCharacters, opt_trailingChars) {
  if(opt_protectEscapedCharacters) {
    str = goog.string.unescapeEntities(str)
  }
  if(opt_trailingChars && str.length > chars) {
    if(opt_trailingChars > chars) {
      opt_trailingChars = chars
    }
    var endPoint = str.length - opt_trailingChars;
    var startPoint = chars - opt_trailingChars;
    str = str.substring(0, startPoint) + "..." + str.substring(endPoint)
  }else {
    if(str.length > chars) {
      var half = Math.floor(chars / 2);
      var endPos = str.length - half;
      half += chars % 2;
      str = str.substring(0, half) + "..." + str.substring(endPos)
    }
  }
  if(opt_protectEscapedCharacters) {
    str = goog.string.htmlEscape(str)
  }
  return str
};
goog.string.specialEscapeChars_ = {"\x00":"\\0", "\u0008":"\\b", "\u000c":"\\f", "\n":"\\n", "\r":"\\r", "\t":"\\t", "\x0B":"\\x0B", '"':'\\"', "\\":"\\\\"};
goog.string.jsEscapeCache_ = {"'":"\\'"};
goog.string.quote = function(s) {
  s = String(s);
  if(s.quote) {
    return s.quote()
  }else {
    var sb = ['"'];
    for(var i = 0;i < s.length;i++) {
      var ch = s.charAt(i);
      var cc = ch.charCodeAt(0);
      sb[i + 1] = goog.string.specialEscapeChars_[ch] || (cc > 31 && cc < 127 ? ch : goog.string.escapeChar(ch))
    }
    sb.push('"');
    return sb.join("")
  }
};
goog.string.escapeString = function(str) {
  var sb = [];
  for(var i = 0;i < str.length;i++) {
    sb[i] = goog.string.escapeChar(str.charAt(i))
  }
  return sb.join("")
};
goog.string.escapeChar = function(c) {
  if(c in goog.string.jsEscapeCache_) {
    return goog.string.jsEscapeCache_[c]
  }
  if(c in goog.string.specialEscapeChars_) {
    return goog.string.jsEscapeCache_[c] = goog.string.specialEscapeChars_[c]
  }
  var rv = c;
  var cc = c.charCodeAt(0);
  if(cc > 31 && cc < 127) {
    rv = c
  }else {
    if(cc < 256) {
      rv = "\\x";
      if(cc < 16 || cc > 256) {
        rv += "0"
      }
    }else {
      rv = "\\u";
      if(cc < 4096) {
        rv += "0"
      }
    }
    rv += cc.toString(16).toUpperCase()
  }
  return goog.string.jsEscapeCache_[c] = rv
};
goog.string.toMap = function(s) {
  var rv = {};
  for(var i = 0;i < s.length;i++) {
    rv[s.charAt(i)] = true
  }
  return rv
};
goog.string.contains = function(s, ss) {
  return s.indexOf(ss) != -1
};
goog.string.removeAt = function(s, index, stringLength) {
  var resultStr = s;
  if(index >= 0 && index < s.length && stringLength > 0) {
    resultStr = s.substr(0, index) + s.substr(index + stringLength, s.length - index - stringLength)
  }
  return resultStr
};
goog.string.remove = function(s, ss) {
  var re = new RegExp(goog.string.regExpEscape(ss), "");
  return s.replace(re, "")
};
goog.string.removeAll = function(s, ss) {
  var re = new RegExp(goog.string.regExpEscape(ss), "g");
  return s.replace(re, "")
};
goog.string.regExpEscape = function(s) {
  return String(s).replace(/([-()\[\]{}+?*.$\^|,:#<!\\])/g, "\\$1").replace(/\x08/g, "\\x08")
};
goog.string.repeat = function(string, length) {
  return(new Array(length + 1)).join(string)
};
goog.string.padNumber = function(num, length, opt_precision) {
  var s = goog.isDef(opt_precision) ? num.toFixed(opt_precision) : String(num);
  var index = s.indexOf(".");
  if(index == -1) {
    index = s.length
  }
  return goog.string.repeat("0", Math.max(0, length - index)) + s
};
goog.string.makeSafe = function(obj) {
  return obj == null ? "" : String(obj)
};
goog.string.buildString = function(var_args) {
  return Array.prototype.join.call(arguments, "")
};
goog.string.getRandomString = function() {
  var x = 2147483648;
  return Math.floor(Math.random() * x).toString(36) + Math.abs(Math.floor(Math.random() * x) ^ goog.now()).toString(36)
};
goog.string.compareVersions = function(version1, version2) {
  var order = 0;
  var v1Subs = goog.string.trim(String(version1)).split(".");
  var v2Subs = goog.string.trim(String(version2)).split(".");
  var subCount = Math.max(v1Subs.length, v2Subs.length);
  for(var subIdx = 0;order == 0 && subIdx < subCount;subIdx++) {
    var v1Sub = v1Subs[subIdx] || "";
    var v2Sub = v2Subs[subIdx] || "";
    var v1CompParser = new RegExp("(\\d*)(\\D*)", "g");
    var v2CompParser = new RegExp("(\\d*)(\\D*)", "g");
    do {
      var v1Comp = v1CompParser.exec(v1Sub) || ["", "", ""];
      var v2Comp = v2CompParser.exec(v2Sub) || ["", "", ""];
      if(v1Comp[0].length == 0 && v2Comp[0].length == 0) {
        break
      }
      var v1CompNum = v1Comp[1].length == 0 ? 0 : parseInt(v1Comp[1], 10);
      var v2CompNum = v2Comp[1].length == 0 ? 0 : parseInt(v2Comp[1], 10);
      order = goog.string.compareElements_(v1CompNum, v2CompNum) || goog.string.compareElements_(v1Comp[2].length == 0, v2Comp[2].length == 0) || goog.string.compareElements_(v1Comp[2], v2Comp[2])
    }while(order == 0)
  }
  return order
};
goog.string.compareElements_ = function(left, right) {
  if(left < right) {
    return-1
  }else {
    if(left > right) {
      return 1
    }
  }
  return 0
};
goog.string.HASHCODE_MAX_ = 4294967296;
goog.string.hashCode = function(str) {
  var result = 0;
  for(var i = 0;i < str.length;++i) {
    result = 31 * result + str.charCodeAt(i);
    result %= goog.string.HASHCODE_MAX_
  }
  return result
};
goog.string.uniqueStringCounter_ = Math.random() * 2147483648 | 0;
goog.string.createUniqueString = function() {
  return"goog_" + goog.string.uniqueStringCounter_++
};
goog.string.toNumber = function(str) {
  var num = Number(str);
  if(num == 0 && goog.string.isEmpty(str)) {
    return NaN
  }
  return num
};
goog.string.toCamelCaseCache_ = {};
goog.string.toCamelCase = function(str) {
  return goog.string.toCamelCaseCache_[str] || (goog.string.toCamelCaseCache_[str] = String(str).replace(/\-([a-z])/g, function(all, match) {
    return match.toUpperCase()
  }))
};
goog.string.toSelectorCaseCache_ = {};
goog.string.toSelectorCase = function(str) {
  return goog.string.toSelectorCaseCache_[str] || (goog.string.toSelectorCaseCache_[str] = String(str).replace(/([A-Z])/g, "-$1").toLowerCase())
};
goog.provide("goog.asserts");
goog.provide("goog.asserts.AssertionError");
goog.require("goog.debug.Error");
goog.require("goog.string");
goog.asserts.ENABLE_ASSERTS = goog.DEBUG;
goog.asserts.AssertionError = function(messagePattern, messageArgs) {
  messageArgs.unshift(messagePattern);
  goog.debug.Error.call(this, goog.string.subs.apply(null, messageArgs));
  messageArgs.shift();
  this.messagePattern = messagePattern
};
goog.inherits(goog.asserts.AssertionError, goog.debug.Error);
goog.asserts.AssertionError.prototype.name = "AssertionError";
goog.asserts.doAssertFailure_ = function(defaultMessage, defaultArgs, givenMessage, givenArgs) {
  var message = "Assertion failed";
  if(givenMessage) {
    message += ": " + givenMessage;
    var args = givenArgs
  }else {
    if(defaultMessage) {
      message += ": " + defaultMessage;
      args = defaultArgs
    }
  }
  throw new goog.asserts.AssertionError("" + message, args || []);
};
goog.asserts.assert = function(condition, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !condition) {
    goog.asserts.doAssertFailure_("", null, opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return condition
};
goog.asserts.fail = function(opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS) {
    throw new goog.asserts.AssertionError("Failure" + (opt_message ? ": " + opt_message : ""), Array.prototype.slice.call(arguments, 1));
  }
};
goog.asserts.assertNumber = function(value, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !goog.isNumber(value)) {
    goog.asserts.doAssertFailure_("Expected number but got %s: %s.", [goog.typeOf(value), value], opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return value
};
goog.asserts.assertString = function(value, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !goog.isString(value)) {
    goog.asserts.doAssertFailure_("Expected string but got %s: %s.", [goog.typeOf(value), value], opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return value
};
goog.asserts.assertFunction = function(value, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !goog.isFunction(value)) {
    goog.asserts.doAssertFailure_("Expected function but got %s: %s.", [goog.typeOf(value), value], opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return value
};
goog.asserts.assertObject = function(value, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !goog.isObject(value)) {
    goog.asserts.doAssertFailure_("Expected object but got %s: %s.", [goog.typeOf(value), value], opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return value
};
goog.asserts.assertArray = function(value, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !goog.isArray(value)) {
    goog.asserts.doAssertFailure_("Expected array but got %s: %s.", [goog.typeOf(value), value], opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return value
};
goog.asserts.assertBoolean = function(value, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !goog.isBoolean(value)) {
    goog.asserts.doAssertFailure_("Expected boolean but got %s: %s.", [goog.typeOf(value), value], opt_message, Array.prototype.slice.call(arguments, 2))
  }
  return value
};
goog.asserts.assertInstanceof = function(value, type, opt_message, var_args) {
  if(goog.asserts.ENABLE_ASSERTS && !(value instanceof type)) {
    goog.asserts.doAssertFailure_("instanceof check failed.", null, opt_message, Array.prototype.slice.call(arguments, 3))
  }
};
goog.provide("goog.array");
goog.provide("goog.array.ArrayLike");
goog.require("goog.asserts");
goog.NATIVE_ARRAY_PROTOTYPES = true;
goog.array.ArrayLike;
goog.array.peek = function(array) {
  return array[array.length - 1]
};
goog.array.ARRAY_PROTOTYPE_ = Array.prototype;
goog.array.indexOf = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.indexOf ? function(arr, obj, opt_fromIndex) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.indexOf.call(arr, obj, opt_fromIndex)
} : function(arr, obj, opt_fromIndex) {
  var fromIndex = opt_fromIndex == null ? 0 : opt_fromIndex < 0 ? Math.max(0, arr.length + opt_fromIndex) : opt_fromIndex;
  if(goog.isString(arr)) {
    if(!goog.isString(obj) || obj.length != 1) {
      return-1
    }
    return arr.indexOf(obj, fromIndex)
  }
  for(var i = fromIndex;i < arr.length;i++) {
    if(i in arr && arr[i] === obj) {
      return i
    }
  }
  return-1
};
goog.array.lastIndexOf = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.lastIndexOf ? function(arr, obj, opt_fromIndex) {
  goog.asserts.assert(arr.length != null);
  var fromIndex = opt_fromIndex == null ? arr.length - 1 : opt_fromIndex;
  return goog.array.ARRAY_PROTOTYPE_.lastIndexOf.call(arr, obj, fromIndex)
} : function(arr, obj, opt_fromIndex) {
  var fromIndex = opt_fromIndex == null ? arr.length - 1 : opt_fromIndex;
  if(fromIndex < 0) {
    fromIndex = Math.max(0, arr.length + fromIndex)
  }
  if(goog.isString(arr)) {
    if(!goog.isString(obj) || obj.length != 1) {
      return-1
    }
    return arr.lastIndexOf(obj, fromIndex)
  }
  for(var i = fromIndex;i >= 0;i--) {
    if(i in arr && arr[i] === obj) {
      return i
    }
  }
  return-1
};
goog.array.forEach = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.forEach ? function(arr, f, opt_obj) {
  goog.asserts.assert(arr.length != null);
  goog.array.ARRAY_PROTOTYPE_.forEach.call(arr, f, opt_obj)
} : function(arr, f, opt_obj) {
  var l = arr.length;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = 0;i < l;i++) {
    if(i in arr2) {
      f.call(opt_obj, arr2[i], i, arr)
    }
  }
};
goog.array.forEachRight = function(arr, f, opt_obj) {
  var l = arr.length;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = l - 1;i >= 0;--i) {
    if(i in arr2) {
      f.call(opt_obj, arr2[i], i, arr)
    }
  }
};
goog.array.filter = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.filter ? function(arr, f, opt_obj) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.filter.call(arr, f, opt_obj)
} : function(arr, f, opt_obj) {
  var l = arr.length;
  var res = [];
  var resLength = 0;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = 0;i < l;i++) {
    if(i in arr2) {
      var val = arr2[i];
      if(f.call(opt_obj, val, i, arr)) {
        res[resLength++] = val
      }
    }
  }
  return res
};
goog.array.map = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.map ? function(arr, f, opt_obj) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.map.call(arr, f, opt_obj)
} : function(arr, f, opt_obj) {
  var l = arr.length;
  var res = new Array(l);
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = 0;i < l;i++) {
    if(i in arr2) {
      res[i] = f.call(opt_obj, arr2[i], i, arr)
    }
  }
  return res
};
goog.array.reduce = function(arr, f, val, opt_obj) {
  if(arr.reduce) {
    if(opt_obj) {
      return arr.reduce(goog.bind(f, opt_obj), val)
    }else {
      return arr.reduce(f, val)
    }
  }
  var rval = val;
  goog.array.forEach(arr, function(val, index) {
    rval = f.call(opt_obj, rval, val, index, arr)
  });
  return rval
};
goog.array.reduceRight = function(arr, f, val, opt_obj) {
  if(arr.reduceRight) {
    if(opt_obj) {
      return arr.reduceRight(goog.bind(f, opt_obj), val)
    }else {
      return arr.reduceRight(f, val)
    }
  }
  var rval = val;
  goog.array.forEachRight(arr, function(val, index) {
    rval = f.call(opt_obj, rval, val, index, arr)
  });
  return rval
};
goog.array.some = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.some ? function(arr, f, opt_obj) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.some.call(arr, f, opt_obj)
} : function(arr, f, opt_obj) {
  var l = arr.length;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = 0;i < l;i++) {
    if(i in arr2 && f.call(opt_obj, arr2[i], i, arr)) {
      return true
    }
  }
  return false
};
goog.array.every = goog.NATIVE_ARRAY_PROTOTYPES && goog.array.ARRAY_PROTOTYPE_.every ? function(arr, f, opt_obj) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.every.call(arr, f, opt_obj)
} : function(arr, f, opt_obj) {
  var l = arr.length;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = 0;i < l;i++) {
    if(i in arr2 && !f.call(opt_obj, arr2[i], i, arr)) {
      return false
    }
  }
  return true
};
goog.array.find = function(arr, f, opt_obj) {
  var i = goog.array.findIndex(arr, f, opt_obj);
  return i < 0 ? null : goog.isString(arr) ? arr.charAt(i) : arr[i]
};
goog.array.findIndex = function(arr, f, opt_obj) {
  var l = arr.length;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = 0;i < l;i++) {
    if(i in arr2 && f.call(opt_obj, arr2[i], i, arr)) {
      return i
    }
  }
  return-1
};
goog.array.findRight = function(arr, f, opt_obj) {
  var i = goog.array.findIndexRight(arr, f, opt_obj);
  return i < 0 ? null : goog.isString(arr) ? arr.charAt(i) : arr[i]
};
goog.array.findIndexRight = function(arr, f, opt_obj) {
  var l = arr.length;
  var arr2 = goog.isString(arr) ? arr.split("") : arr;
  for(var i = l - 1;i >= 0;i--) {
    if(i in arr2 && f.call(opt_obj, arr2[i], i, arr)) {
      return i
    }
  }
  return-1
};
goog.array.contains = function(arr, obj) {
  return goog.array.indexOf(arr, obj) >= 0
};
goog.array.isEmpty = function(arr) {
  return arr.length == 0
};
goog.array.clear = function(arr) {
  if(!goog.isArray(arr)) {
    for(var i = arr.length - 1;i >= 0;i--) {
      delete arr[i]
    }
  }
  arr.length = 0
};
goog.array.insert = function(arr, obj) {
  if(!goog.array.contains(arr, obj)) {
    arr.push(obj)
  }
};
goog.array.insertAt = function(arr, obj, opt_i) {
  goog.array.splice(arr, opt_i, 0, obj)
};
goog.array.insertArrayAt = function(arr, elementsToAdd, opt_i) {
  goog.partial(goog.array.splice, arr, opt_i, 0).apply(null, elementsToAdd)
};
goog.array.insertBefore = function(arr, obj, opt_obj2) {
  var i;
  if(arguments.length == 2 || (i = goog.array.indexOf(arr, opt_obj2)) < 0) {
    arr.push(obj)
  }else {
    goog.array.insertAt(arr, obj, i)
  }
};
goog.array.remove = function(arr, obj) {
  var i = goog.array.indexOf(arr, obj);
  var rv;
  if(rv = i >= 0) {
    goog.array.removeAt(arr, i)
  }
  return rv
};
goog.array.removeAt = function(arr, i) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.splice.call(arr, i, 1).length == 1
};
goog.array.removeIf = function(arr, f, opt_obj) {
  var i = goog.array.findIndex(arr, f, opt_obj);
  if(i >= 0) {
    goog.array.removeAt(arr, i);
    return true
  }
  return false
};
goog.array.concat = function(var_args) {
  return goog.array.ARRAY_PROTOTYPE_.concat.apply(goog.array.ARRAY_PROTOTYPE_, arguments)
};
goog.array.clone = function(arr) {
  if(goog.isArray(arr)) {
    return goog.array.concat(arr)
  }else {
    var rv = [];
    for(var i = 0, len = arr.length;i < len;i++) {
      rv[i] = arr[i]
    }
    return rv
  }
};
goog.array.toArray = function(object) {
  if(goog.isArray(object)) {
    return goog.array.concat(object)
  }
  return goog.array.clone(object)
};
goog.array.extend = function(arr1, var_args) {
  for(var i = 1;i < arguments.length;i++) {
    var arr2 = arguments[i];
    var isArrayLike;
    if(goog.isArray(arr2) || (isArrayLike = goog.isArrayLike(arr2)) && arr2.hasOwnProperty("callee")) {
      arr1.push.apply(arr1, arr2)
    }else {
      if(isArrayLike) {
        var len1 = arr1.length;
        var len2 = arr2.length;
        for(var j = 0;j < len2;j++) {
          arr1[len1 + j] = arr2[j]
        }
      }else {
        arr1.push(arr2)
      }
    }
  }
};
goog.array.splice = function(arr, index, howMany, var_args) {
  goog.asserts.assert(arr.length != null);
  return goog.array.ARRAY_PROTOTYPE_.splice.apply(arr, goog.array.slice(arguments, 1))
};
goog.array.slice = function(arr, start, opt_end) {
  goog.asserts.assert(arr.length != null);
  if(arguments.length <= 2) {
    return goog.array.ARRAY_PROTOTYPE_.slice.call(arr, start)
  }else {
    return goog.array.ARRAY_PROTOTYPE_.slice.call(arr, start, opt_end)
  }
};
goog.array.removeDuplicates = function(arr, opt_rv) {
  var returnArray = opt_rv || arr;
  var seen = {}, cursorInsert = 0, cursorRead = 0;
  while(cursorRead < arr.length) {
    var current = arr[cursorRead++];
    var key = goog.isObject(current) ? "o" + goog.getUid(current) : (typeof current).charAt(0) + current;
    if(!Object.prototype.hasOwnProperty.call(seen, key)) {
      seen[key] = true;
      returnArray[cursorInsert++] = current
    }
  }
  returnArray.length = cursorInsert
};
goog.array.binarySearch = function(arr, target, opt_compareFn) {
  return goog.array.binarySearch_(arr, opt_compareFn || goog.array.defaultCompare, false, target)
};
goog.array.binarySelect = function(arr, evaluator, opt_obj) {
  return goog.array.binarySearch_(arr, evaluator, true, undefined, opt_obj)
};
goog.array.binarySearch_ = function(arr, compareFn, isEvaluator, opt_target, opt_selfObj) {
  var left = 0;
  var right = arr.length;
  var found;
  while(left < right) {
    var middle = left + right >> 1;
    var compareResult;
    if(isEvaluator) {
      compareResult = compareFn.call(opt_selfObj, arr[middle], middle, arr)
    }else {
      compareResult = compareFn(opt_target, arr[middle])
    }
    if(compareResult > 0) {
      left = middle + 1
    }else {
      right = middle;
      found = !compareResult
    }
  }
  return found ? left : ~left
};
goog.array.sort = function(arr, opt_compareFn) {
  goog.asserts.assert(arr.length != null);
  goog.array.ARRAY_PROTOTYPE_.sort.call(arr, opt_compareFn || goog.array.defaultCompare)
};
goog.array.stableSort = function(arr, opt_compareFn) {
  for(var i = 0;i < arr.length;i++) {
    arr[i] = {index:i, value:arr[i]}
  }
  var valueCompareFn = opt_compareFn || goog.array.defaultCompare;
  function stableCompareFn(obj1, obj2) {
    return valueCompareFn(obj1.value, obj2.value) || obj1.index - obj2.index
  }
  goog.array.sort(arr, stableCompareFn);
  for(var i = 0;i < arr.length;i++) {
    arr[i] = arr[i].value
  }
};
goog.array.sortObjectsByKey = function(arr, key, opt_compareFn) {
  var compare = opt_compareFn || goog.array.defaultCompare;
  goog.array.sort(arr, function(a, b) {
    return compare(a[key], b[key])
  })
};
goog.array.isSorted = function(arr, opt_compareFn, opt_strict) {
  var compare = opt_compareFn || goog.array.defaultCompare;
  for(var i = 1;i < arr.length;i++) {
    var compareResult = compare(arr[i - 1], arr[i]);
    if(compareResult > 0 || compareResult == 0 && opt_strict) {
      return false
    }
  }
  return true
};
goog.array.equals = function(arr1, arr2, opt_equalsFn) {
  if(!goog.isArrayLike(arr1) || !goog.isArrayLike(arr2) || arr1.length != arr2.length) {
    return false
  }
  var l = arr1.length;
  var equalsFn = opt_equalsFn || goog.array.defaultCompareEquality;
  for(var i = 0;i < l;i++) {
    if(!equalsFn(arr1[i], arr2[i])) {
      return false
    }
  }
  return true
};
goog.array.compare = function(arr1, arr2, opt_equalsFn) {
  return goog.array.equals(arr1, arr2, opt_equalsFn)
};
goog.array.compare3 = function(arr1, arr2, opt_compareFn) {
  var compare = opt_compareFn || goog.array.defaultCompare;
  var l = Math.min(arr1.length, arr2.length);
  for(var i = 0;i < l;i++) {
    var result = compare(arr1[i], arr2[i]);
    if(result != 0) {
      return result
    }
  }
  return goog.array.defaultCompare(arr1.length, arr2.length)
};
goog.array.defaultCompare = function(a, b) {
  return a > b ? 1 : a < b ? -1 : 0
};
goog.array.defaultCompareEquality = function(a, b) {
  return a === b
};
goog.array.binaryInsert = function(array, value, opt_compareFn) {
  var index = goog.array.binarySearch(array, value, opt_compareFn);
  if(index < 0) {
    goog.array.insertAt(array, value, -(index + 1));
    return true
  }
  return false
};
goog.array.binaryRemove = function(array, value, opt_compareFn) {
  var index = goog.array.binarySearch(array, value, opt_compareFn);
  return index >= 0 ? goog.array.removeAt(array, index) : false
};
goog.array.bucket = function(array, sorter) {
  var buckets = {};
  for(var i = 0;i < array.length;i++) {
    var value = array[i];
    var key = sorter(value, i, array);
    if(goog.isDef(key)) {
      var bucket = buckets[key] || (buckets[key] = []);
      bucket.push(value)
    }
  }
  return buckets
};
goog.array.repeat = function(value, n) {
  var array = [];
  for(var i = 0;i < n;i++) {
    array[i] = value
  }
  return array
};
goog.array.flatten = function(var_args) {
  var result = [];
  for(var i = 0;i < arguments.length;i++) {
    var element = arguments[i];
    if(goog.isArray(element)) {
      result.push.apply(result, goog.array.flatten.apply(null, element))
    }else {
      result.push(element)
    }
  }
  return result
};
goog.array.rotate = function(array, n) {
  goog.asserts.assert(array.length != null);
  if(array.length) {
    n %= array.length;
    if(n > 0) {
      goog.array.ARRAY_PROTOTYPE_.unshift.apply(array, array.splice(-n, n))
    }else {
      if(n < 0) {
        goog.array.ARRAY_PROTOTYPE_.push.apply(array, array.splice(0, -n))
      }
    }
  }
  return array
};
goog.array.zip = function(var_args) {
  if(!arguments.length) {
    return[]
  }
  var result = [];
  for(var i = 0;true;i++) {
    var value = [];
    for(var j = 0;j < arguments.length;j++) {
      var arr = arguments[j];
      if(i >= arr.length) {
        return result
      }
      value.push(arr[i])
    }
    result.push(value)
  }
};
goog.array.shuffle = function(arr, opt_randFn) {
  var randFn = opt_randFn || Math.random;
  for(var i = arr.length - 1;i > 0;i--) {
    var j = Math.floor(randFn() * (i + 1));
    var tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp
  }
};
goog.provide("goog.object");
goog.object.forEach = function(obj, f, opt_obj) {
  for(var key in obj) {
    f.call(opt_obj, obj[key], key, obj)
  }
};
goog.object.filter = function(obj, f, opt_obj) {
  var res = {};
  for(var key in obj) {
    if(f.call(opt_obj, obj[key], key, obj)) {
      res[key] = obj[key]
    }
  }
  return res
};
goog.object.map = function(obj, f, opt_obj) {
  var res = {};
  for(var key in obj) {
    res[key] = f.call(opt_obj, obj[key], key, obj)
  }
  return res
};
goog.object.some = function(obj, f, opt_obj) {
  for(var key in obj) {
    if(f.call(opt_obj, obj[key], key, obj)) {
      return true
    }
  }
  return false
};
goog.object.every = function(obj, f, opt_obj) {
  for(var key in obj) {
    if(!f.call(opt_obj, obj[key], key, obj)) {
      return false
    }
  }
  return true
};
goog.object.getCount = function(obj) {
  var rv = 0;
  for(var key in obj) {
    rv++
  }
  return rv
};
goog.object.getAnyKey = function(obj) {
  for(var key in obj) {
    return key
  }
};
goog.object.getAnyValue = function(obj) {
  for(var key in obj) {
    return obj[key]
  }
};
goog.object.contains = function(obj, val) {
  return goog.object.containsValue(obj, val)
};
goog.object.getValues = function(obj) {
  var res = [];
  var i = 0;
  for(var key in obj) {
    res[i++] = obj[key]
  }
  return res
};
goog.object.getKeys = function(obj) {
  var res = [];
  var i = 0;
  for(var key in obj) {
    res[i++] = key
  }
  return res
};
goog.object.getValueByKeys = function(obj, var_args) {
  var isArrayLike = goog.isArrayLike(var_args);
  var keys = isArrayLike ? var_args : arguments;
  for(var i = isArrayLike ? 0 : 1;i < keys.length;i++) {
    obj = obj[keys[i]];
    if(!goog.isDef(obj)) {
      break
    }
  }
  return obj
};
goog.object.containsKey = function(obj, key) {
  return key in obj
};
goog.object.containsValue = function(obj, val) {
  for(var key in obj) {
    if(obj[key] == val) {
      return true
    }
  }
  return false
};
goog.object.findKey = function(obj, f, opt_this) {
  for(var key in obj) {
    if(f.call(opt_this, obj[key], key, obj)) {
      return key
    }
  }
  return undefined
};
goog.object.findValue = function(obj, f, opt_this) {
  var key = goog.object.findKey(obj, f, opt_this);
  return key && obj[key]
};
goog.object.isEmpty = function(obj) {
  for(var key in obj) {
    return false
  }
  return true
};
goog.object.clear = function(obj) {
  for(var i in obj) {
    delete obj[i]
  }
};
goog.object.remove = function(obj, key) {
  var rv;
  if(rv = key in obj) {
    delete obj[key]
  }
  return rv
};
goog.object.add = function(obj, key, val) {
  if(key in obj) {
    throw Error('The object already contains the key "' + key + '"');
  }
  goog.object.set(obj, key, val)
};
goog.object.get = function(obj, key, opt_val) {
  if(key in obj) {
    return obj[key]
  }
  return opt_val
};
goog.object.set = function(obj, key, value) {
  obj[key] = value
};
goog.object.setIfUndefined = function(obj, key, value) {
  return key in obj ? obj[key] : obj[key] = value
};
goog.object.clone = function(obj) {
  var res = {};
  for(var key in obj) {
    res[key] = obj[key]
  }
  return res
};
goog.object.unsafeClone = function(obj) {
  var type = goog.typeOf(obj);
  if(type == "object" || type == "array") {
    if(obj.clone) {
      return obj.clone()
    }
    var clone = type == "array" ? [] : {};
    for(var key in obj) {
      clone[key] = goog.object.unsafeClone(obj[key])
    }
    return clone
  }
  return obj
};
goog.object.transpose = function(obj) {
  var transposed = {};
  for(var key in obj) {
    transposed[obj[key]] = key
  }
  return transposed
};
goog.object.PROTOTYPE_FIELDS_ = ["constructor", "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable", "toLocaleString", "toString", "valueOf"];
goog.object.extend = function(target, var_args) {
  var key, source;
  for(var i = 1;i < arguments.length;i++) {
    source = arguments[i];
    for(key in source) {
      target[key] = source[key]
    }
    for(var j = 0;j < goog.object.PROTOTYPE_FIELDS_.length;j++) {
      key = goog.object.PROTOTYPE_FIELDS_[j];
      if(Object.prototype.hasOwnProperty.call(source, key)) {
        target[key] = source[key]
      }
    }
  }
};
goog.object.create = function(var_args) {
  var argLength = arguments.length;
  if(argLength == 1 && goog.isArray(arguments[0])) {
    return goog.object.create.apply(null, arguments[0])
  }
  if(argLength % 2) {
    throw Error("Uneven number of arguments");
  }
  var rv = {};
  for(var i = 0;i < argLength;i += 2) {
    rv[arguments[i]] = arguments[i + 1]
  }
  return rv
};
goog.object.createSet = function(var_args) {
  var argLength = arguments.length;
  if(argLength == 1 && goog.isArray(arguments[0])) {
    return goog.object.createSet.apply(null, arguments[0])
  }
  var rv = {};
  for(var i = 0;i < argLength;i++) {
    rv[arguments[i]] = true
  }
  return rv
};
goog.provide("goog.string.format");
goog.require("goog.string");
goog.string.format = function(formatString, var_args) {
  var args = Array.prototype.slice.call(arguments);
  var template = args.shift();
  if(typeof template == "undefined") {
    throw Error("[goog.string.format] Template required");
  }
  var formatRe = /%([0\-\ \+]*)(\d+)?(\.(\d+))?([%sfdiu])/g;
  function replacerDemuxer(match, flags, width, dotp, precision, type, offset, wholeString) {
    if(type == "%") {
      return"%"
    }
    var value = args.shift();
    if(typeof value == "undefined") {
      throw Error("[goog.string.format] Not enough arguments");
    }
    arguments[0] = value;
    return goog.string.format.demuxes_[type].apply(null, arguments)
  }
  return template.replace(formatRe, replacerDemuxer)
};
goog.string.format.demuxes_ = {};
goog.string.format.demuxes_["s"] = function(value, flags, width, dotp, precision, type, offset, wholeString) {
  var replacement = value;
  if(isNaN(width) || width == "" || replacement.length >= width) {
    return replacement
  }
  if(flags.indexOf("-", 0) > -1) {
    replacement = replacement + goog.string.repeat(" ", width - replacement.length)
  }else {
    replacement = goog.string.repeat(" ", width - replacement.length) + replacement
  }
  return replacement
};
goog.string.format.demuxes_["f"] = function(value, flags, width, dotp, precision, type, offset, wholeString) {
  var replacement = value.toString();
  if(!(isNaN(precision) || precision == "")) {
    replacement = value.toFixed(precision)
  }
  var sign;
  if(value < 0) {
    sign = "-"
  }else {
    if(flags.indexOf("+") >= 0) {
      sign = "+"
    }else {
      if(flags.indexOf(" ") >= 0) {
        sign = " "
      }else {
        sign = ""
      }
    }
  }
  if(value >= 0) {
    replacement = sign + replacement
  }
  if(isNaN(width) || replacement.length >= width) {
    return replacement
  }
  replacement = isNaN(precision) ? Math.abs(value).toString() : Math.abs(value).toFixed(precision);
  var padCount = width - replacement.length - sign.length;
  if(flags.indexOf("-", 0) >= 0) {
    replacement = sign + replacement + goog.string.repeat(" ", padCount)
  }else {
    var paddingChar = flags.indexOf("0", 0) >= 0 ? "0" : " ";
    replacement = sign + goog.string.repeat(paddingChar, padCount) + replacement
  }
  return replacement
};
goog.string.format.demuxes_["d"] = function(value, flags, width, dotp, precision, type, offset, wholeString) {
  return goog.string.format.demuxes_["f"](parseInt(value, 10), flags, width, dotp, 0, type, offset, wholeString)
};
goog.string.format.demuxes_["i"] = goog.string.format.demuxes_["d"];
goog.string.format.demuxes_["u"] = goog.string.format.demuxes_["d"];
goog.provide("goog.userAgent.jscript");
goog.require("goog.string");
goog.userAgent.jscript.ASSUME_NO_JSCRIPT = false;
goog.userAgent.jscript.init_ = function() {
  var hasScriptEngine = "ScriptEngine" in goog.global;
  goog.userAgent.jscript.DETECTED_HAS_JSCRIPT_ = hasScriptEngine && goog.global["ScriptEngine"]() == "JScript";
  goog.userAgent.jscript.DETECTED_VERSION_ = goog.userAgent.jscript.DETECTED_HAS_JSCRIPT_ ? goog.global["ScriptEngineMajorVersion"]() + "." + goog.global["ScriptEngineMinorVersion"]() + "." + goog.global["ScriptEngineBuildVersion"]() : "0"
};
if(!goog.userAgent.jscript.ASSUME_NO_JSCRIPT) {
  goog.userAgent.jscript.init_()
}
goog.userAgent.jscript.HAS_JSCRIPT = goog.userAgent.jscript.ASSUME_NO_JSCRIPT ? false : goog.userAgent.jscript.DETECTED_HAS_JSCRIPT_;
goog.userAgent.jscript.VERSION = goog.userAgent.jscript.ASSUME_NO_JSCRIPT ? "0" : goog.userAgent.jscript.DETECTED_VERSION_;
goog.userAgent.jscript.isVersion = function(version) {
  return goog.string.compareVersions(goog.userAgent.jscript.VERSION, version) >= 0
};
goog.provide("goog.string.StringBuffer");
goog.require("goog.userAgent.jscript");
goog.string.StringBuffer = function(opt_a1, var_args) {
  this.buffer_ = goog.userAgent.jscript.HAS_JSCRIPT ? [] : "";
  if(opt_a1 != null) {
    this.append.apply(this, arguments)
  }
};
goog.string.StringBuffer.prototype.set = function(s) {
  this.clear();
  this.append(s)
};
if(goog.userAgent.jscript.HAS_JSCRIPT) {
  goog.string.StringBuffer.prototype.bufferLength_ = 0;
  goog.string.StringBuffer.prototype.append = function(a1, opt_a2, var_args) {
    if(opt_a2 == null) {
      this.buffer_[this.bufferLength_++] = a1
    }else {
      this.buffer_.push.apply(this.buffer_, arguments);
      this.bufferLength_ = this.buffer_.length
    }
    return this
  }
}else {
  goog.string.StringBuffer.prototype.append = function(a1, opt_a2, var_args) {
    this.buffer_ += a1;
    if(opt_a2 != null) {
      for(var i = 1;i < arguments.length;i++) {
        this.buffer_ += arguments[i]
      }
    }
    return this
  }
}
goog.string.StringBuffer.prototype.clear = function() {
  if(goog.userAgent.jscript.HAS_JSCRIPT) {
    this.buffer_.length = 0;
    this.bufferLength_ = 0
  }else {
    this.buffer_ = ""
  }
};
goog.string.StringBuffer.prototype.getLength = function() {
  return this.toString().length
};
goog.string.StringBuffer.prototype.toString = function() {
  if(goog.userAgent.jscript.HAS_JSCRIPT) {
    var str = this.buffer_.join("");
    this.clear();
    if(str) {
      this.append(str)
    }
    return str
  }else {
    return this.buffer_
  }
};
goog.provide("cljs.core");
goog.require("goog.array");
goog.require("goog.object");
goog.require("goog.string.format");
goog.require("goog.string.StringBuffer");
goog.require("goog.string");
cljs.core._STAR_unchecked_if_STAR_ = false;
cljs.core._STAR_print_fn_STAR_ = function _STAR_print_fn_STAR_(_) {
  throw new Error("No *print-fn* fn set for evaluation environment");
};
cljs.core.truth_ = function truth_(x) {
  return x != null && x !== false
};
cljs.core.identical_QMARK_ = function identical_QMARK_(x, y) {
  return x === y
};
cljs.core.nil_QMARK_ = function nil_QMARK_(x) {
  return x == null
};
cljs.core.not = function not(x) {
  if(cljs.core.truth_(x)) {
    return false
  }else {
    return true
  }
};
cljs.core.type_satisfies_ = function type_satisfies_(p, x) {
  var x__16310 = x == null ? null : x;
  if(p[goog.typeOf(x__16310)]) {
    return true
  }else {
    if(p["_"]) {
      return true
    }else {
      if("\ufdd0'else") {
        return false
      }else {
        return null
      }
    }
  }
};
cljs.core.is_proto_ = function is_proto_(x) {
  return x.constructor.prototype === x
};
cljs.core._STAR_main_cli_fn_STAR_ = null;
cljs.core.missing_protocol = function missing_protocol(proto, obj) {
  return Error(["No protocol method ", proto, " defined for type ", goog.typeOf(obj), ": ", obj].join(""))
};
cljs.core.aclone = function aclone(array_like) {
  return array_like.slice()
};
cljs.core.array = function array(var_args) {
  return Array.prototype.slice.call(arguments)
};
cljs.core.make_array = function() {
  var make_array = null;
  var make_array__1 = function(size) {
    return new Array(size)
  };
  var make_array__2 = function(type, size) {
    return make_array.call(null, size)
  };
  make_array = function(type, size) {
    switch(arguments.length) {
      case 1:
        return make_array__1.call(this, type);
      case 2:
        return make_array__2.call(this, type, size)
    }
    throw"Invalid arity: " + arguments.length;
  };
  make_array.cljs$lang$arity$1 = make_array__1;
  make_array.cljs$lang$arity$2 = make_array__2;
  return make_array
}();
cljs.core.aget = function() {
  var aget = null;
  var aget__2 = function(array, i) {
    return array[i]
  };
  var aget__3 = function() {
    var G__16311__delegate = function(array, i, idxs) {
      return cljs.core.apply.call(null, aget, aget.call(null, array, i), idxs)
    };
    var G__16311 = function(array, i, var_args) {
      var idxs = null;
      if(goog.isDef(var_args)) {
        idxs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__16311__delegate.call(this, array, i, idxs)
    };
    G__16311.cljs$lang$maxFixedArity = 2;
    G__16311.cljs$lang$applyTo = function(arglist__16312) {
      var array = cljs.core.first(arglist__16312);
      var i = cljs.core.first(cljs.core.next(arglist__16312));
      var idxs = cljs.core.rest(cljs.core.next(arglist__16312));
      return G__16311__delegate(array, i, idxs)
    };
    G__16311.cljs$lang$arity$variadic = G__16311__delegate;
    return G__16311
  }();
  aget = function(array, i, var_args) {
    var idxs = var_args;
    switch(arguments.length) {
      case 2:
        return aget__2.call(this, array, i);
      default:
        return aget__3.cljs$lang$arity$variadic(array, i, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  aget.cljs$lang$maxFixedArity = 2;
  aget.cljs$lang$applyTo = aget__3.cljs$lang$applyTo;
  aget.cljs$lang$arity$2 = aget__2;
  aget.cljs$lang$arity$variadic = aget__3.cljs$lang$arity$variadic;
  return aget
}();
cljs.core.aset = function aset(array, i, val) {
  return array[i] = val
};
cljs.core.alength = function alength(array) {
  return array.length
};
cljs.core.into_array = function() {
  var into_array = null;
  var into_array__1 = function(aseq) {
    return into_array.call(null, null, aseq)
  };
  var into_array__2 = function(type, aseq) {
    return cljs.core.reduce.call(null, function(a, x) {
      a.push(x);
      return a
    }, [], aseq)
  };
  into_array = function(type, aseq) {
    switch(arguments.length) {
      case 1:
        return into_array__1.call(this, type);
      case 2:
        return into_array__2.call(this, type, aseq)
    }
    throw"Invalid arity: " + arguments.length;
  };
  into_array.cljs$lang$arity$1 = into_array__1;
  into_array.cljs$lang$arity$2 = into_array__2;
  return into_array
}();
cljs.core.IFn = {};
cljs.core._invoke = function() {
  var _invoke = null;
  var _invoke__1 = function(this$) {
    if(function() {
      var and__3822__auto____16397 = this$;
      if(and__3822__auto____16397) {
        return this$.cljs$core$IFn$_invoke$arity$1
      }else {
        return and__3822__auto____16397
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$1(this$)
    }else {
      var x__2431__auto____16398 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16399 = cljs.core._invoke[goog.typeOf(x__2431__auto____16398)];
        if(or__3824__auto____16399) {
          return or__3824__auto____16399
        }else {
          var or__3824__auto____16400 = cljs.core._invoke["_"];
          if(or__3824__auto____16400) {
            return or__3824__auto____16400
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$)
    }
  };
  var _invoke__2 = function(this$, a) {
    if(function() {
      var and__3822__auto____16401 = this$;
      if(and__3822__auto____16401) {
        return this$.cljs$core$IFn$_invoke$arity$2
      }else {
        return and__3822__auto____16401
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$2(this$, a)
    }else {
      var x__2431__auto____16402 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16403 = cljs.core._invoke[goog.typeOf(x__2431__auto____16402)];
        if(or__3824__auto____16403) {
          return or__3824__auto____16403
        }else {
          var or__3824__auto____16404 = cljs.core._invoke["_"];
          if(or__3824__auto____16404) {
            return or__3824__auto____16404
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a)
    }
  };
  var _invoke__3 = function(this$, a, b) {
    if(function() {
      var and__3822__auto____16405 = this$;
      if(and__3822__auto____16405) {
        return this$.cljs$core$IFn$_invoke$arity$3
      }else {
        return and__3822__auto____16405
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$3(this$, a, b)
    }else {
      var x__2431__auto____16406 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16407 = cljs.core._invoke[goog.typeOf(x__2431__auto____16406)];
        if(or__3824__auto____16407) {
          return or__3824__auto____16407
        }else {
          var or__3824__auto____16408 = cljs.core._invoke["_"];
          if(or__3824__auto____16408) {
            return or__3824__auto____16408
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b)
    }
  };
  var _invoke__4 = function(this$, a, b, c) {
    if(function() {
      var and__3822__auto____16409 = this$;
      if(and__3822__auto____16409) {
        return this$.cljs$core$IFn$_invoke$arity$4
      }else {
        return and__3822__auto____16409
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$4(this$, a, b, c)
    }else {
      var x__2431__auto____16410 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16411 = cljs.core._invoke[goog.typeOf(x__2431__auto____16410)];
        if(or__3824__auto____16411) {
          return or__3824__auto____16411
        }else {
          var or__3824__auto____16412 = cljs.core._invoke["_"];
          if(or__3824__auto____16412) {
            return or__3824__auto____16412
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c)
    }
  };
  var _invoke__5 = function(this$, a, b, c, d) {
    if(function() {
      var and__3822__auto____16413 = this$;
      if(and__3822__auto____16413) {
        return this$.cljs$core$IFn$_invoke$arity$5
      }else {
        return and__3822__auto____16413
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$5(this$, a, b, c, d)
    }else {
      var x__2431__auto____16414 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16415 = cljs.core._invoke[goog.typeOf(x__2431__auto____16414)];
        if(or__3824__auto____16415) {
          return or__3824__auto____16415
        }else {
          var or__3824__auto____16416 = cljs.core._invoke["_"];
          if(or__3824__auto____16416) {
            return or__3824__auto____16416
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d)
    }
  };
  var _invoke__6 = function(this$, a, b, c, d, e) {
    if(function() {
      var and__3822__auto____16417 = this$;
      if(and__3822__auto____16417) {
        return this$.cljs$core$IFn$_invoke$arity$6
      }else {
        return and__3822__auto____16417
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$6(this$, a, b, c, d, e)
    }else {
      var x__2431__auto____16418 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16419 = cljs.core._invoke[goog.typeOf(x__2431__auto____16418)];
        if(or__3824__auto____16419) {
          return or__3824__auto____16419
        }else {
          var or__3824__auto____16420 = cljs.core._invoke["_"];
          if(or__3824__auto____16420) {
            return or__3824__auto____16420
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e)
    }
  };
  var _invoke__7 = function(this$, a, b, c, d, e, f) {
    if(function() {
      var and__3822__auto____16421 = this$;
      if(and__3822__auto____16421) {
        return this$.cljs$core$IFn$_invoke$arity$7
      }else {
        return and__3822__auto____16421
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$7(this$, a, b, c, d, e, f)
    }else {
      var x__2431__auto____16422 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16423 = cljs.core._invoke[goog.typeOf(x__2431__auto____16422)];
        if(or__3824__auto____16423) {
          return or__3824__auto____16423
        }else {
          var or__3824__auto____16424 = cljs.core._invoke["_"];
          if(or__3824__auto____16424) {
            return or__3824__auto____16424
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f)
    }
  };
  var _invoke__8 = function(this$, a, b, c, d, e, f, g) {
    if(function() {
      var and__3822__auto____16425 = this$;
      if(and__3822__auto____16425) {
        return this$.cljs$core$IFn$_invoke$arity$8
      }else {
        return and__3822__auto____16425
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$8(this$, a, b, c, d, e, f, g)
    }else {
      var x__2431__auto____16426 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16427 = cljs.core._invoke[goog.typeOf(x__2431__auto____16426)];
        if(or__3824__auto____16427) {
          return or__3824__auto____16427
        }else {
          var or__3824__auto____16428 = cljs.core._invoke["_"];
          if(or__3824__auto____16428) {
            return or__3824__auto____16428
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g)
    }
  };
  var _invoke__9 = function(this$, a, b, c, d, e, f, g, h) {
    if(function() {
      var and__3822__auto____16429 = this$;
      if(and__3822__auto____16429) {
        return this$.cljs$core$IFn$_invoke$arity$9
      }else {
        return and__3822__auto____16429
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$9(this$, a, b, c, d, e, f, g, h)
    }else {
      var x__2431__auto____16430 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16431 = cljs.core._invoke[goog.typeOf(x__2431__auto____16430)];
        if(or__3824__auto____16431) {
          return or__3824__auto____16431
        }else {
          var or__3824__auto____16432 = cljs.core._invoke["_"];
          if(or__3824__auto____16432) {
            return or__3824__auto____16432
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h)
    }
  };
  var _invoke__10 = function(this$, a, b, c, d, e, f, g, h, i) {
    if(function() {
      var and__3822__auto____16433 = this$;
      if(and__3822__auto____16433) {
        return this$.cljs$core$IFn$_invoke$arity$10
      }else {
        return and__3822__auto____16433
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$10(this$, a, b, c, d, e, f, g, h, i)
    }else {
      var x__2431__auto____16434 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16435 = cljs.core._invoke[goog.typeOf(x__2431__auto____16434)];
        if(or__3824__auto____16435) {
          return or__3824__auto____16435
        }else {
          var or__3824__auto____16436 = cljs.core._invoke["_"];
          if(or__3824__auto____16436) {
            return or__3824__auto____16436
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i)
    }
  };
  var _invoke__11 = function(this$, a, b, c, d, e, f, g, h, i, j) {
    if(function() {
      var and__3822__auto____16437 = this$;
      if(and__3822__auto____16437) {
        return this$.cljs$core$IFn$_invoke$arity$11
      }else {
        return and__3822__auto____16437
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$11(this$, a, b, c, d, e, f, g, h, i, j)
    }else {
      var x__2431__auto____16438 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16439 = cljs.core._invoke[goog.typeOf(x__2431__auto____16438)];
        if(or__3824__auto____16439) {
          return or__3824__auto____16439
        }else {
          var or__3824__auto____16440 = cljs.core._invoke["_"];
          if(or__3824__auto____16440) {
            return or__3824__auto____16440
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j)
    }
  };
  var _invoke__12 = function(this$, a, b, c, d, e, f, g, h, i, j, k) {
    if(function() {
      var and__3822__auto____16441 = this$;
      if(and__3822__auto____16441) {
        return this$.cljs$core$IFn$_invoke$arity$12
      }else {
        return and__3822__auto____16441
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$12(this$, a, b, c, d, e, f, g, h, i, j, k)
    }else {
      var x__2431__auto____16442 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16443 = cljs.core._invoke[goog.typeOf(x__2431__auto____16442)];
        if(or__3824__auto____16443) {
          return or__3824__auto____16443
        }else {
          var or__3824__auto____16444 = cljs.core._invoke["_"];
          if(or__3824__auto____16444) {
            return or__3824__auto____16444
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k)
    }
  };
  var _invoke__13 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l) {
    if(function() {
      var and__3822__auto____16445 = this$;
      if(and__3822__auto____16445) {
        return this$.cljs$core$IFn$_invoke$arity$13
      }else {
        return and__3822__auto____16445
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$13(this$, a, b, c, d, e, f, g, h, i, j, k, l)
    }else {
      var x__2431__auto____16446 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16447 = cljs.core._invoke[goog.typeOf(x__2431__auto____16446)];
        if(or__3824__auto____16447) {
          return or__3824__auto____16447
        }else {
          var or__3824__auto____16448 = cljs.core._invoke["_"];
          if(or__3824__auto____16448) {
            return or__3824__auto____16448
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l)
    }
  };
  var _invoke__14 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m) {
    if(function() {
      var and__3822__auto____16449 = this$;
      if(and__3822__auto____16449) {
        return this$.cljs$core$IFn$_invoke$arity$14
      }else {
        return and__3822__auto____16449
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$14(this$, a, b, c, d, e, f, g, h, i, j, k, l, m)
    }else {
      var x__2431__auto____16450 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16451 = cljs.core._invoke[goog.typeOf(x__2431__auto____16450)];
        if(or__3824__auto____16451) {
          return or__3824__auto____16451
        }else {
          var or__3824__auto____16452 = cljs.core._invoke["_"];
          if(or__3824__auto____16452) {
            return or__3824__auto____16452
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m)
    }
  };
  var _invoke__15 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n) {
    if(function() {
      var and__3822__auto____16453 = this$;
      if(and__3822__auto____16453) {
        return this$.cljs$core$IFn$_invoke$arity$15
      }else {
        return and__3822__auto____16453
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$15(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    }else {
      var x__2431__auto____16454 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16455 = cljs.core._invoke[goog.typeOf(x__2431__auto____16454)];
        if(or__3824__auto____16455) {
          return or__3824__auto____16455
        }else {
          var or__3824__auto____16456 = cljs.core._invoke["_"];
          if(or__3824__auto____16456) {
            return or__3824__auto____16456
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    }
  };
  var _invoke__16 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) {
    if(function() {
      var and__3822__auto____16457 = this$;
      if(and__3822__auto____16457) {
        return this$.cljs$core$IFn$_invoke$arity$16
      }else {
        return and__3822__auto____16457
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$16(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }else {
      var x__2431__auto____16458 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16459 = cljs.core._invoke[goog.typeOf(x__2431__auto____16458)];
        if(or__3824__auto____16459) {
          return or__3824__auto____16459
        }else {
          var or__3824__auto____16460 = cljs.core._invoke["_"];
          if(or__3824__auto____16460) {
            return or__3824__auto____16460
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }
  };
  var _invoke__17 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) {
    if(function() {
      var and__3822__auto____16461 = this$;
      if(and__3822__auto____16461) {
        return this$.cljs$core$IFn$_invoke$arity$17
      }else {
        return and__3822__auto____16461
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$17(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    }else {
      var x__2431__auto____16462 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16463 = cljs.core._invoke[goog.typeOf(x__2431__auto____16462)];
        if(or__3824__auto____16463) {
          return or__3824__auto____16463
        }else {
          var or__3824__auto____16464 = cljs.core._invoke["_"];
          if(or__3824__auto____16464) {
            return or__3824__auto____16464
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    }
  };
  var _invoke__18 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) {
    if(function() {
      var and__3822__auto____16465 = this$;
      if(and__3822__auto____16465) {
        return this$.cljs$core$IFn$_invoke$arity$18
      }else {
        return and__3822__auto____16465
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$18(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }else {
      var x__2431__auto____16466 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16467 = cljs.core._invoke[goog.typeOf(x__2431__auto____16466)];
        if(or__3824__auto____16467) {
          return or__3824__auto____16467
        }else {
          var or__3824__auto____16468 = cljs.core._invoke["_"];
          if(or__3824__auto____16468) {
            return or__3824__auto____16468
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }
  };
  var _invoke__19 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s) {
    if(function() {
      var and__3822__auto____16469 = this$;
      if(and__3822__auto____16469) {
        return this$.cljs$core$IFn$_invoke$arity$19
      }else {
        return and__3822__auto____16469
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$19(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s)
    }else {
      var x__2431__auto____16470 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16471 = cljs.core._invoke[goog.typeOf(x__2431__auto____16470)];
        if(or__3824__auto____16471) {
          return or__3824__auto____16471
        }else {
          var or__3824__auto____16472 = cljs.core._invoke["_"];
          if(or__3824__auto____16472) {
            return or__3824__auto____16472
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s)
    }
  };
  var _invoke__20 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t) {
    if(function() {
      var and__3822__auto____16473 = this$;
      if(and__3822__auto____16473) {
        return this$.cljs$core$IFn$_invoke$arity$20
      }else {
        return and__3822__auto____16473
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$20(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t)
    }else {
      var x__2431__auto____16474 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16475 = cljs.core._invoke[goog.typeOf(x__2431__auto____16474)];
        if(or__3824__auto____16475) {
          return or__3824__auto____16475
        }else {
          var or__3824__auto____16476 = cljs.core._invoke["_"];
          if(or__3824__auto____16476) {
            return or__3824__auto____16476
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t)
    }
  };
  var _invoke__21 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest) {
    if(function() {
      var and__3822__auto____16477 = this$;
      if(and__3822__auto____16477) {
        return this$.cljs$core$IFn$_invoke$arity$21
      }else {
        return and__3822__auto____16477
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$21(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest)
    }else {
      var x__2431__auto____16478 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16479 = cljs.core._invoke[goog.typeOf(x__2431__auto____16478)];
        if(or__3824__auto____16479) {
          return or__3824__auto____16479
        }else {
          var or__3824__auto____16480 = cljs.core._invoke["_"];
          if(or__3824__auto____16480) {
            return or__3824__auto____16480
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest)
    }
  };
  _invoke = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest) {
    switch(arguments.length) {
      case 1:
        return _invoke__1.call(this, this$);
      case 2:
        return _invoke__2.call(this, this$, a);
      case 3:
        return _invoke__3.call(this, this$, a, b);
      case 4:
        return _invoke__4.call(this, this$, a, b, c);
      case 5:
        return _invoke__5.call(this, this$, a, b, c, d);
      case 6:
        return _invoke__6.call(this, this$, a, b, c, d, e);
      case 7:
        return _invoke__7.call(this, this$, a, b, c, d, e, f);
      case 8:
        return _invoke__8.call(this, this$, a, b, c, d, e, f, g);
      case 9:
        return _invoke__9.call(this, this$, a, b, c, d, e, f, g, h);
      case 10:
        return _invoke__10.call(this, this$, a, b, c, d, e, f, g, h, i);
      case 11:
        return _invoke__11.call(this, this$, a, b, c, d, e, f, g, h, i, j);
      case 12:
        return _invoke__12.call(this, this$, a, b, c, d, e, f, g, h, i, j, k);
      case 13:
        return _invoke__13.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l);
      case 14:
        return _invoke__14.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m);
      case 15:
        return _invoke__15.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n);
      case 16:
        return _invoke__16.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o);
      case 17:
        return _invoke__17.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p);
      case 18:
        return _invoke__18.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q);
      case 19:
        return _invoke__19.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s);
      case 20:
        return _invoke__20.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t);
      case 21:
        return _invoke__21.call(this, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest)
    }
    throw"Invalid arity: " + arguments.length;
  };
  _invoke.cljs$lang$arity$1 = _invoke__1;
  _invoke.cljs$lang$arity$2 = _invoke__2;
  _invoke.cljs$lang$arity$3 = _invoke__3;
  _invoke.cljs$lang$arity$4 = _invoke__4;
  _invoke.cljs$lang$arity$5 = _invoke__5;
  _invoke.cljs$lang$arity$6 = _invoke__6;
  _invoke.cljs$lang$arity$7 = _invoke__7;
  _invoke.cljs$lang$arity$8 = _invoke__8;
  _invoke.cljs$lang$arity$9 = _invoke__9;
  _invoke.cljs$lang$arity$10 = _invoke__10;
  _invoke.cljs$lang$arity$11 = _invoke__11;
  _invoke.cljs$lang$arity$12 = _invoke__12;
  _invoke.cljs$lang$arity$13 = _invoke__13;
  _invoke.cljs$lang$arity$14 = _invoke__14;
  _invoke.cljs$lang$arity$15 = _invoke__15;
  _invoke.cljs$lang$arity$16 = _invoke__16;
  _invoke.cljs$lang$arity$17 = _invoke__17;
  _invoke.cljs$lang$arity$18 = _invoke__18;
  _invoke.cljs$lang$arity$19 = _invoke__19;
  _invoke.cljs$lang$arity$20 = _invoke__20;
  _invoke.cljs$lang$arity$21 = _invoke__21;
  return _invoke
}();
cljs.core.ICounted = {};
cljs.core._count = function _count(coll) {
  if(function() {
    var and__3822__auto____16485 = coll;
    if(and__3822__auto____16485) {
      return coll.cljs$core$ICounted$_count$arity$1
    }else {
      return and__3822__auto____16485
    }
  }()) {
    return coll.cljs$core$ICounted$_count$arity$1(coll)
  }else {
    var x__2431__auto____16486 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16487 = cljs.core._count[goog.typeOf(x__2431__auto____16486)];
      if(or__3824__auto____16487) {
        return or__3824__auto____16487
      }else {
        var or__3824__auto____16488 = cljs.core._count["_"];
        if(or__3824__auto____16488) {
          return or__3824__auto____16488
        }else {
          throw cljs.core.missing_protocol.call(null, "ICounted.-count", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.IEmptyableCollection = {};
cljs.core._empty = function _empty(coll) {
  if(function() {
    var and__3822__auto____16493 = coll;
    if(and__3822__auto____16493) {
      return coll.cljs$core$IEmptyableCollection$_empty$arity$1
    }else {
      return and__3822__auto____16493
    }
  }()) {
    return coll.cljs$core$IEmptyableCollection$_empty$arity$1(coll)
  }else {
    var x__2431__auto____16494 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16495 = cljs.core._empty[goog.typeOf(x__2431__auto____16494)];
      if(or__3824__auto____16495) {
        return or__3824__auto____16495
      }else {
        var or__3824__auto____16496 = cljs.core._empty["_"];
        if(or__3824__auto____16496) {
          return or__3824__auto____16496
        }else {
          throw cljs.core.missing_protocol.call(null, "IEmptyableCollection.-empty", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.ICollection = {};
cljs.core._conj = function _conj(coll, o) {
  if(function() {
    var and__3822__auto____16501 = coll;
    if(and__3822__auto____16501) {
      return coll.cljs$core$ICollection$_conj$arity$2
    }else {
      return and__3822__auto____16501
    }
  }()) {
    return coll.cljs$core$ICollection$_conj$arity$2(coll, o)
  }else {
    var x__2431__auto____16502 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16503 = cljs.core._conj[goog.typeOf(x__2431__auto____16502)];
      if(or__3824__auto____16503) {
        return or__3824__auto____16503
      }else {
        var or__3824__auto____16504 = cljs.core._conj["_"];
        if(or__3824__auto____16504) {
          return or__3824__auto____16504
        }else {
          throw cljs.core.missing_protocol.call(null, "ICollection.-conj", coll);
        }
      }
    }().call(null, coll, o)
  }
};
cljs.core.IIndexed = {};
cljs.core._nth = function() {
  var _nth = null;
  var _nth__2 = function(coll, n) {
    if(function() {
      var and__3822__auto____16513 = coll;
      if(and__3822__auto____16513) {
        return coll.cljs$core$IIndexed$_nth$arity$2
      }else {
        return and__3822__auto____16513
      }
    }()) {
      return coll.cljs$core$IIndexed$_nth$arity$2(coll, n)
    }else {
      var x__2431__auto____16514 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____16515 = cljs.core._nth[goog.typeOf(x__2431__auto____16514)];
        if(or__3824__auto____16515) {
          return or__3824__auto____16515
        }else {
          var or__3824__auto____16516 = cljs.core._nth["_"];
          if(or__3824__auto____16516) {
            return or__3824__auto____16516
          }else {
            throw cljs.core.missing_protocol.call(null, "IIndexed.-nth", coll);
          }
        }
      }().call(null, coll, n)
    }
  };
  var _nth__3 = function(coll, n, not_found) {
    if(function() {
      var and__3822__auto____16517 = coll;
      if(and__3822__auto____16517) {
        return coll.cljs$core$IIndexed$_nth$arity$3
      }else {
        return and__3822__auto____16517
      }
    }()) {
      return coll.cljs$core$IIndexed$_nth$arity$3(coll, n, not_found)
    }else {
      var x__2431__auto____16518 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____16519 = cljs.core._nth[goog.typeOf(x__2431__auto____16518)];
        if(or__3824__auto____16519) {
          return or__3824__auto____16519
        }else {
          var or__3824__auto____16520 = cljs.core._nth["_"];
          if(or__3824__auto____16520) {
            return or__3824__auto____16520
          }else {
            throw cljs.core.missing_protocol.call(null, "IIndexed.-nth", coll);
          }
        }
      }().call(null, coll, n, not_found)
    }
  };
  _nth = function(coll, n, not_found) {
    switch(arguments.length) {
      case 2:
        return _nth__2.call(this, coll, n);
      case 3:
        return _nth__3.call(this, coll, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  _nth.cljs$lang$arity$2 = _nth__2;
  _nth.cljs$lang$arity$3 = _nth__3;
  return _nth
}();
cljs.core.ASeq = {};
cljs.core.ISeq = {};
cljs.core._first = function _first(coll) {
  if(function() {
    var and__3822__auto____16525 = coll;
    if(and__3822__auto____16525) {
      return coll.cljs$core$ISeq$_first$arity$1
    }else {
      return and__3822__auto____16525
    }
  }()) {
    return coll.cljs$core$ISeq$_first$arity$1(coll)
  }else {
    var x__2431__auto____16526 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16527 = cljs.core._first[goog.typeOf(x__2431__auto____16526)];
      if(or__3824__auto____16527) {
        return or__3824__auto____16527
      }else {
        var or__3824__auto____16528 = cljs.core._first["_"];
        if(or__3824__auto____16528) {
          return or__3824__auto____16528
        }else {
          throw cljs.core.missing_protocol.call(null, "ISeq.-first", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._rest = function _rest(coll) {
  if(function() {
    var and__3822__auto____16533 = coll;
    if(and__3822__auto____16533) {
      return coll.cljs$core$ISeq$_rest$arity$1
    }else {
      return and__3822__auto____16533
    }
  }()) {
    return coll.cljs$core$ISeq$_rest$arity$1(coll)
  }else {
    var x__2431__auto____16534 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16535 = cljs.core._rest[goog.typeOf(x__2431__auto____16534)];
      if(or__3824__auto____16535) {
        return or__3824__auto____16535
      }else {
        var or__3824__auto____16536 = cljs.core._rest["_"];
        if(or__3824__auto____16536) {
          return or__3824__auto____16536
        }else {
          throw cljs.core.missing_protocol.call(null, "ISeq.-rest", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.INext = {};
cljs.core._next = function _next(coll) {
  if(function() {
    var and__3822__auto____16541 = coll;
    if(and__3822__auto____16541) {
      return coll.cljs$core$INext$_next$arity$1
    }else {
      return and__3822__auto____16541
    }
  }()) {
    return coll.cljs$core$INext$_next$arity$1(coll)
  }else {
    var x__2431__auto____16542 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16543 = cljs.core._next[goog.typeOf(x__2431__auto____16542)];
      if(or__3824__auto____16543) {
        return or__3824__auto____16543
      }else {
        var or__3824__auto____16544 = cljs.core._next["_"];
        if(or__3824__auto____16544) {
          return or__3824__auto____16544
        }else {
          throw cljs.core.missing_protocol.call(null, "INext.-next", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.ILookup = {};
cljs.core._lookup = function() {
  var _lookup = null;
  var _lookup__2 = function(o, k) {
    if(function() {
      var and__3822__auto____16553 = o;
      if(and__3822__auto____16553) {
        return o.cljs$core$ILookup$_lookup$arity$2
      }else {
        return and__3822__auto____16553
      }
    }()) {
      return o.cljs$core$ILookup$_lookup$arity$2(o, k)
    }else {
      var x__2431__auto____16554 = o == null ? null : o;
      return function() {
        var or__3824__auto____16555 = cljs.core._lookup[goog.typeOf(x__2431__auto____16554)];
        if(or__3824__auto____16555) {
          return or__3824__auto____16555
        }else {
          var or__3824__auto____16556 = cljs.core._lookup["_"];
          if(or__3824__auto____16556) {
            return or__3824__auto____16556
          }else {
            throw cljs.core.missing_protocol.call(null, "ILookup.-lookup", o);
          }
        }
      }().call(null, o, k)
    }
  };
  var _lookup__3 = function(o, k, not_found) {
    if(function() {
      var and__3822__auto____16557 = o;
      if(and__3822__auto____16557) {
        return o.cljs$core$ILookup$_lookup$arity$3
      }else {
        return and__3822__auto____16557
      }
    }()) {
      return o.cljs$core$ILookup$_lookup$arity$3(o, k, not_found)
    }else {
      var x__2431__auto____16558 = o == null ? null : o;
      return function() {
        var or__3824__auto____16559 = cljs.core._lookup[goog.typeOf(x__2431__auto____16558)];
        if(or__3824__auto____16559) {
          return or__3824__auto____16559
        }else {
          var or__3824__auto____16560 = cljs.core._lookup["_"];
          if(or__3824__auto____16560) {
            return or__3824__auto____16560
          }else {
            throw cljs.core.missing_protocol.call(null, "ILookup.-lookup", o);
          }
        }
      }().call(null, o, k, not_found)
    }
  };
  _lookup = function(o, k, not_found) {
    switch(arguments.length) {
      case 2:
        return _lookup__2.call(this, o, k);
      case 3:
        return _lookup__3.call(this, o, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  _lookup.cljs$lang$arity$2 = _lookup__2;
  _lookup.cljs$lang$arity$3 = _lookup__3;
  return _lookup
}();
cljs.core.IAssociative = {};
cljs.core._contains_key_QMARK_ = function _contains_key_QMARK_(coll, k) {
  if(function() {
    var and__3822__auto____16565 = coll;
    if(and__3822__auto____16565) {
      return coll.cljs$core$IAssociative$_contains_key_QMARK_$arity$2
    }else {
      return and__3822__auto____16565
    }
  }()) {
    return coll.cljs$core$IAssociative$_contains_key_QMARK_$arity$2(coll, k)
  }else {
    var x__2431__auto____16566 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16567 = cljs.core._contains_key_QMARK_[goog.typeOf(x__2431__auto____16566)];
      if(or__3824__auto____16567) {
        return or__3824__auto____16567
      }else {
        var or__3824__auto____16568 = cljs.core._contains_key_QMARK_["_"];
        if(or__3824__auto____16568) {
          return or__3824__auto____16568
        }else {
          throw cljs.core.missing_protocol.call(null, "IAssociative.-contains-key?", coll);
        }
      }
    }().call(null, coll, k)
  }
};
cljs.core._assoc = function _assoc(coll, k, v) {
  if(function() {
    var and__3822__auto____16573 = coll;
    if(and__3822__auto____16573) {
      return coll.cljs$core$IAssociative$_assoc$arity$3
    }else {
      return and__3822__auto____16573
    }
  }()) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, k, v)
  }else {
    var x__2431__auto____16574 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16575 = cljs.core._assoc[goog.typeOf(x__2431__auto____16574)];
      if(or__3824__auto____16575) {
        return or__3824__auto____16575
      }else {
        var or__3824__auto____16576 = cljs.core._assoc["_"];
        if(or__3824__auto____16576) {
          return or__3824__auto____16576
        }else {
          throw cljs.core.missing_protocol.call(null, "IAssociative.-assoc", coll);
        }
      }
    }().call(null, coll, k, v)
  }
};
cljs.core.IMap = {};
cljs.core._dissoc = function _dissoc(coll, k) {
  if(function() {
    var and__3822__auto____16581 = coll;
    if(and__3822__auto____16581) {
      return coll.cljs$core$IMap$_dissoc$arity$2
    }else {
      return and__3822__auto____16581
    }
  }()) {
    return coll.cljs$core$IMap$_dissoc$arity$2(coll, k)
  }else {
    var x__2431__auto____16582 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16583 = cljs.core._dissoc[goog.typeOf(x__2431__auto____16582)];
      if(or__3824__auto____16583) {
        return or__3824__auto____16583
      }else {
        var or__3824__auto____16584 = cljs.core._dissoc["_"];
        if(or__3824__auto____16584) {
          return or__3824__auto____16584
        }else {
          throw cljs.core.missing_protocol.call(null, "IMap.-dissoc", coll);
        }
      }
    }().call(null, coll, k)
  }
};
cljs.core.IMapEntry = {};
cljs.core._key = function _key(coll) {
  if(function() {
    var and__3822__auto____16589 = coll;
    if(and__3822__auto____16589) {
      return coll.cljs$core$IMapEntry$_key$arity$1
    }else {
      return and__3822__auto____16589
    }
  }()) {
    return coll.cljs$core$IMapEntry$_key$arity$1(coll)
  }else {
    var x__2431__auto____16590 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16591 = cljs.core._key[goog.typeOf(x__2431__auto____16590)];
      if(or__3824__auto____16591) {
        return or__3824__auto____16591
      }else {
        var or__3824__auto____16592 = cljs.core._key["_"];
        if(or__3824__auto____16592) {
          return or__3824__auto____16592
        }else {
          throw cljs.core.missing_protocol.call(null, "IMapEntry.-key", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._val = function _val(coll) {
  if(function() {
    var and__3822__auto____16597 = coll;
    if(and__3822__auto____16597) {
      return coll.cljs$core$IMapEntry$_val$arity$1
    }else {
      return and__3822__auto____16597
    }
  }()) {
    return coll.cljs$core$IMapEntry$_val$arity$1(coll)
  }else {
    var x__2431__auto____16598 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16599 = cljs.core._val[goog.typeOf(x__2431__auto____16598)];
      if(or__3824__auto____16599) {
        return or__3824__auto____16599
      }else {
        var or__3824__auto____16600 = cljs.core._val["_"];
        if(or__3824__auto____16600) {
          return or__3824__auto____16600
        }else {
          throw cljs.core.missing_protocol.call(null, "IMapEntry.-val", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.ISet = {};
cljs.core._disjoin = function _disjoin(coll, v) {
  if(function() {
    var and__3822__auto____16605 = coll;
    if(and__3822__auto____16605) {
      return coll.cljs$core$ISet$_disjoin$arity$2
    }else {
      return and__3822__auto____16605
    }
  }()) {
    return coll.cljs$core$ISet$_disjoin$arity$2(coll, v)
  }else {
    var x__2431__auto____16606 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16607 = cljs.core._disjoin[goog.typeOf(x__2431__auto____16606)];
      if(or__3824__auto____16607) {
        return or__3824__auto____16607
      }else {
        var or__3824__auto____16608 = cljs.core._disjoin["_"];
        if(or__3824__auto____16608) {
          return or__3824__auto____16608
        }else {
          throw cljs.core.missing_protocol.call(null, "ISet.-disjoin", coll);
        }
      }
    }().call(null, coll, v)
  }
};
cljs.core.IStack = {};
cljs.core._peek = function _peek(coll) {
  if(function() {
    var and__3822__auto____16613 = coll;
    if(and__3822__auto____16613) {
      return coll.cljs$core$IStack$_peek$arity$1
    }else {
      return and__3822__auto____16613
    }
  }()) {
    return coll.cljs$core$IStack$_peek$arity$1(coll)
  }else {
    var x__2431__auto____16614 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16615 = cljs.core._peek[goog.typeOf(x__2431__auto____16614)];
      if(or__3824__auto____16615) {
        return or__3824__auto____16615
      }else {
        var or__3824__auto____16616 = cljs.core._peek["_"];
        if(or__3824__auto____16616) {
          return or__3824__auto____16616
        }else {
          throw cljs.core.missing_protocol.call(null, "IStack.-peek", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._pop = function _pop(coll) {
  if(function() {
    var and__3822__auto____16621 = coll;
    if(and__3822__auto____16621) {
      return coll.cljs$core$IStack$_pop$arity$1
    }else {
      return and__3822__auto____16621
    }
  }()) {
    return coll.cljs$core$IStack$_pop$arity$1(coll)
  }else {
    var x__2431__auto____16622 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16623 = cljs.core._pop[goog.typeOf(x__2431__auto____16622)];
      if(or__3824__auto____16623) {
        return or__3824__auto____16623
      }else {
        var or__3824__auto____16624 = cljs.core._pop["_"];
        if(or__3824__auto____16624) {
          return or__3824__auto____16624
        }else {
          throw cljs.core.missing_protocol.call(null, "IStack.-pop", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.IVector = {};
cljs.core._assoc_n = function _assoc_n(coll, n, val) {
  if(function() {
    var and__3822__auto____16629 = coll;
    if(and__3822__auto____16629) {
      return coll.cljs$core$IVector$_assoc_n$arity$3
    }else {
      return and__3822__auto____16629
    }
  }()) {
    return coll.cljs$core$IVector$_assoc_n$arity$3(coll, n, val)
  }else {
    var x__2431__auto____16630 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16631 = cljs.core._assoc_n[goog.typeOf(x__2431__auto____16630)];
      if(or__3824__auto____16631) {
        return or__3824__auto____16631
      }else {
        var or__3824__auto____16632 = cljs.core._assoc_n["_"];
        if(or__3824__auto____16632) {
          return or__3824__auto____16632
        }else {
          throw cljs.core.missing_protocol.call(null, "IVector.-assoc-n", coll);
        }
      }
    }().call(null, coll, n, val)
  }
};
cljs.core.IDeref = {};
cljs.core._deref = function _deref(o) {
  if(function() {
    var and__3822__auto____16637 = o;
    if(and__3822__auto____16637) {
      return o.cljs$core$IDeref$_deref$arity$1
    }else {
      return and__3822__auto____16637
    }
  }()) {
    return o.cljs$core$IDeref$_deref$arity$1(o)
  }else {
    var x__2431__auto____16638 = o == null ? null : o;
    return function() {
      var or__3824__auto____16639 = cljs.core._deref[goog.typeOf(x__2431__auto____16638)];
      if(or__3824__auto____16639) {
        return or__3824__auto____16639
      }else {
        var or__3824__auto____16640 = cljs.core._deref["_"];
        if(or__3824__auto____16640) {
          return or__3824__auto____16640
        }else {
          throw cljs.core.missing_protocol.call(null, "IDeref.-deref", o);
        }
      }
    }().call(null, o)
  }
};
cljs.core.IDerefWithTimeout = {};
cljs.core._deref_with_timeout = function _deref_with_timeout(o, msec, timeout_val) {
  if(function() {
    var and__3822__auto____16645 = o;
    if(and__3822__auto____16645) {
      return o.cljs$core$IDerefWithTimeout$_deref_with_timeout$arity$3
    }else {
      return and__3822__auto____16645
    }
  }()) {
    return o.cljs$core$IDerefWithTimeout$_deref_with_timeout$arity$3(o, msec, timeout_val)
  }else {
    var x__2431__auto____16646 = o == null ? null : o;
    return function() {
      var or__3824__auto____16647 = cljs.core._deref_with_timeout[goog.typeOf(x__2431__auto____16646)];
      if(or__3824__auto____16647) {
        return or__3824__auto____16647
      }else {
        var or__3824__auto____16648 = cljs.core._deref_with_timeout["_"];
        if(or__3824__auto____16648) {
          return or__3824__auto____16648
        }else {
          throw cljs.core.missing_protocol.call(null, "IDerefWithTimeout.-deref-with-timeout", o);
        }
      }
    }().call(null, o, msec, timeout_val)
  }
};
cljs.core.IMeta = {};
cljs.core._meta = function _meta(o) {
  if(function() {
    var and__3822__auto____16653 = o;
    if(and__3822__auto____16653) {
      return o.cljs$core$IMeta$_meta$arity$1
    }else {
      return and__3822__auto____16653
    }
  }()) {
    return o.cljs$core$IMeta$_meta$arity$1(o)
  }else {
    var x__2431__auto____16654 = o == null ? null : o;
    return function() {
      var or__3824__auto____16655 = cljs.core._meta[goog.typeOf(x__2431__auto____16654)];
      if(or__3824__auto____16655) {
        return or__3824__auto____16655
      }else {
        var or__3824__auto____16656 = cljs.core._meta["_"];
        if(or__3824__auto____16656) {
          return or__3824__auto____16656
        }else {
          throw cljs.core.missing_protocol.call(null, "IMeta.-meta", o);
        }
      }
    }().call(null, o)
  }
};
cljs.core.IWithMeta = {};
cljs.core._with_meta = function _with_meta(o, meta) {
  if(function() {
    var and__3822__auto____16661 = o;
    if(and__3822__auto____16661) {
      return o.cljs$core$IWithMeta$_with_meta$arity$2
    }else {
      return and__3822__auto____16661
    }
  }()) {
    return o.cljs$core$IWithMeta$_with_meta$arity$2(o, meta)
  }else {
    var x__2431__auto____16662 = o == null ? null : o;
    return function() {
      var or__3824__auto____16663 = cljs.core._with_meta[goog.typeOf(x__2431__auto____16662)];
      if(or__3824__auto____16663) {
        return or__3824__auto____16663
      }else {
        var or__3824__auto____16664 = cljs.core._with_meta["_"];
        if(or__3824__auto____16664) {
          return or__3824__auto____16664
        }else {
          throw cljs.core.missing_protocol.call(null, "IWithMeta.-with-meta", o);
        }
      }
    }().call(null, o, meta)
  }
};
cljs.core.IReduce = {};
cljs.core._reduce = function() {
  var _reduce = null;
  var _reduce__2 = function(coll, f) {
    if(function() {
      var and__3822__auto____16673 = coll;
      if(and__3822__auto____16673) {
        return coll.cljs$core$IReduce$_reduce$arity$2
      }else {
        return and__3822__auto____16673
      }
    }()) {
      return coll.cljs$core$IReduce$_reduce$arity$2(coll, f)
    }else {
      var x__2431__auto____16674 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____16675 = cljs.core._reduce[goog.typeOf(x__2431__auto____16674)];
        if(or__3824__auto____16675) {
          return or__3824__auto____16675
        }else {
          var or__3824__auto____16676 = cljs.core._reduce["_"];
          if(or__3824__auto____16676) {
            return or__3824__auto____16676
          }else {
            throw cljs.core.missing_protocol.call(null, "IReduce.-reduce", coll);
          }
        }
      }().call(null, coll, f)
    }
  };
  var _reduce__3 = function(coll, f, start) {
    if(function() {
      var and__3822__auto____16677 = coll;
      if(and__3822__auto____16677) {
        return coll.cljs$core$IReduce$_reduce$arity$3
      }else {
        return and__3822__auto____16677
      }
    }()) {
      return coll.cljs$core$IReduce$_reduce$arity$3(coll, f, start)
    }else {
      var x__2431__auto____16678 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____16679 = cljs.core._reduce[goog.typeOf(x__2431__auto____16678)];
        if(or__3824__auto____16679) {
          return or__3824__auto____16679
        }else {
          var or__3824__auto____16680 = cljs.core._reduce["_"];
          if(or__3824__auto____16680) {
            return or__3824__auto____16680
          }else {
            throw cljs.core.missing_protocol.call(null, "IReduce.-reduce", coll);
          }
        }
      }().call(null, coll, f, start)
    }
  };
  _reduce = function(coll, f, start) {
    switch(arguments.length) {
      case 2:
        return _reduce__2.call(this, coll, f);
      case 3:
        return _reduce__3.call(this, coll, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  _reduce.cljs$lang$arity$2 = _reduce__2;
  _reduce.cljs$lang$arity$3 = _reduce__3;
  return _reduce
}();
cljs.core.IKVReduce = {};
cljs.core._kv_reduce = function _kv_reduce(coll, f, init) {
  if(function() {
    var and__3822__auto____16685 = coll;
    if(and__3822__auto____16685) {
      return coll.cljs$core$IKVReduce$_kv_reduce$arity$3
    }else {
      return and__3822__auto____16685
    }
  }()) {
    return coll.cljs$core$IKVReduce$_kv_reduce$arity$3(coll, f, init)
  }else {
    var x__2431__auto____16686 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16687 = cljs.core._kv_reduce[goog.typeOf(x__2431__auto____16686)];
      if(or__3824__auto____16687) {
        return or__3824__auto____16687
      }else {
        var or__3824__auto____16688 = cljs.core._kv_reduce["_"];
        if(or__3824__auto____16688) {
          return or__3824__auto____16688
        }else {
          throw cljs.core.missing_protocol.call(null, "IKVReduce.-kv-reduce", coll);
        }
      }
    }().call(null, coll, f, init)
  }
};
cljs.core.IEquiv = {};
cljs.core._equiv = function _equiv(o, other) {
  if(function() {
    var and__3822__auto____16693 = o;
    if(and__3822__auto____16693) {
      return o.cljs$core$IEquiv$_equiv$arity$2
    }else {
      return and__3822__auto____16693
    }
  }()) {
    return o.cljs$core$IEquiv$_equiv$arity$2(o, other)
  }else {
    var x__2431__auto____16694 = o == null ? null : o;
    return function() {
      var or__3824__auto____16695 = cljs.core._equiv[goog.typeOf(x__2431__auto____16694)];
      if(or__3824__auto____16695) {
        return or__3824__auto____16695
      }else {
        var or__3824__auto____16696 = cljs.core._equiv["_"];
        if(or__3824__auto____16696) {
          return or__3824__auto____16696
        }else {
          throw cljs.core.missing_protocol.call(null, "IEquiv.-equiv", o);
        }
      }
    }().call(null, o, other)
  }
};
cljs.core.IHash = {};
cljs.core._hash = function _hash(o) {
  if(function() {
    var and__3822__auto____16701 = o;
    if(and__3822__auto____16701) {
      return o.cljs$core$IHash$_hash$arity$1
    }else {
      return and__3822__auto____16701
    }
  }()) {
    return o.cljs$core$IHash$_hash$arity$1(o)
  }else {
    var x__2431__auto____16702 = o == null ? null : o;
    return function() {
      var or__3824__auto____16703 = cljs.core._hash[goog.typeOf(x__2431__auto____16702)];
      if(or__3824__auto____16703) {
        return or__3824__auto____16703
      }else {
        var or__3824__auto____16704 = cljs.core._hash["_"];
        if(or__3824__auto____16704) {
          return or__3824__auto____16704
        }else {
          throw cljs.core.missing_protocol.call(null, "IHash.-hash", o);
        }
      }
    }().call(null, o)
  }
};
cljs.core.ISeqable = {};
cljs.core._seq = function _seq(o) {
  if(function() {
    var and__3822__auto____16709 = o;
    if(and__3822__auto____16709) {
      return o.cljs$core$ISeqable$_seq$arity$1
    }else {
      return and__3822__auto____16709
    }
  }()) {
    return o.cljs$core$ISeqable$_seq$arity$1(o)
  }else {
    var x__2431__auto____16710 = o == null ? null : o;
    return function() {
      var or__3824__auto____16711 = cljs.core._seq[goog.typeOf(x__2431__auto____16710)];
      if(or__3824__auto____16711) {
        return or__3824__auto____16711
      }else {
        var or__3824__auto____16712 = cljs.core._seq["_"];
        if(or__3824__auto____16712) {
          return or__3824__auto____16712
        }else {
          throw cljs.core.missing_protocol.call(null, "ISeqable.-seq", o);
        }
      }
    }().call(null, o)
  }
};
cljs.core.ISequential = {};
cljs.core.IList = {};
cljs.core.IRecord = {};
cljs.core.IReversible = {};
cljs.core._rseq = function _rseq(coll) {
  if(function() {
    var and__3822__auto____16717 = coll;
    if(and__3822__auto____16717) {
      return coll.cljs$core$IReversible$_rseq$arity$1
    }else {
      return and__3822__auto____16717
    }
  }()) {
    return coll.cljs$core$IReversible$_rseq$arity$1(coll)
  }else {
    var x__2431__auto____16718 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16719 = cljs.core._rseq[goog.typeOf(x__2431__auto____16718)];
      if(or__3824__auto____16719) {
        return or__3824__auto____16719
      }else {
        var or__3824__auto____16720 = cljs.core._rseq["_"];
        if(or__3824__auto____16720) {
          return or__3824__auto____16720
        }else {
          throw cljs.core.missing_protocol.call(null, "IReversible.-rseq", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.ISorted = {};
cljs.core._sorted_seq = function _sorted_seq(coll, ascending_QMARK_) {
  if(function() {
    var and__3822__auto____16725 = coll;
    if(and__3822__auto____16725) {
      return coll.cljs$core$ISorted$_sorted_seq$arity$2
    }else {
      return and__3822__auto____16725
    }
  }()) {
    return coll.cljs$core$ISorted$_sorted_seq$arity$2(coll, ascending_QMARK_)
  }else {
    var x__2431__auto____16726 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16727 = cljs.core._sorted_seq[goog.typeOf(x__2431__auto____16726)];
      if(or__3824__auto____16727) {
        return or__3824__auto____16727
      }else {
        var or__3824__auto____16728 = cljs.core._sorted_seq["_"];
        if(or__3824__auto____16728) {
          return or__3824__auto____16728
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-sorted-seq", coll);
        }
      }
    }().call(null, coll, ascending_QMARK_)
  }
};
cljs.core._sorted_seq_from = function _sorted_seq_from(coll, k, ascending_QMARK_) {
  if(function() {
    var and__3822__auto____16733 = coll;
    if(and__3822__auto____16733) {
      return coll.cljs$core$ISorted$_sorted_seq_from$arity$3
    }else {
      return and__3822__auto____16733
    }
  }()) {
    return coll.cljs$core$ISorted$_sorted_seq_from$arity$3(coll, k, ascending_QMARK_)
  }else {
    var x__2431__auto____16734 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16735 = cljs.core._sorted_seq_from[goog.typeOf(x__2431__auto____16734)];
      if(or__3824__auto____16735) {
        return or__3824__auto____16735
      }else {
        var or__3824__auto____16736 = cljs.core._sorted_seq_from["_"];
        if(or__3824__auto____16736) {
          return or__3824__auto____16736
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-sorted-seq-from", coll);
        }
      }
    }().call(null, coll, k, ascending_QMARK_)
  }
};
cljs.core._entry_key = function _entry_key(coll, entry) {
  if(function() {
    var and__3822__auto____16741 = coll;
    if(and__3822__auto____16741) {
      return coll.cljs$core$ISorted$_entry_key$arity$2
    }else {
      return and__3822__auto____16741
    }
  }()) {
    return coll.cljs$core$ISorted$_entry_key$arity$2(coll, entry)
  }else {
    var x__2431__auto____16742 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16743 = cljs.core._entry_key[goog.typeOf(x__2431__auto____16742)];
      if(or__3824__auto____16743) {
        return or__3824__auto____16743
      }else {
        var or__3824__auto____16744 = cljs.core._entry_key["_"];
        if(or__3824__auto____16744) {
          return or__3824__auto____16744
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-entry-key", coll);
        }
      }
    }().call(null, coll, entry)
  }
};
cljs.core._comparator = function _comparator(coll) {
  if(function() {
    var and__3822__auto____16749 = coll;
    if(and__3822__auto____16749) {
      return coll.cljs$core$ISorted$_comparator$arity$1
    }else {
      return and__3822__auto____16749
    }
  }()) {
    return coll.cljs$core$ISorted$_comparator$arity$1(coll)
  }else {
    var x__2431__auto____16750 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16751 = cljs.core._comparator[goog.typeOf(x__2431__auto____16750)];
      if(or__3824__auto____16751) {
        return or__3824__auto____16751
      }else {
        var or__3824__auto____16752 = cljs.core._comparator["_"];
        if(or__3824__auto____16752) {
          return or__3824__auto____16752
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-comparator", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.IPrintable = {};
cljs.core._pr_seq = function _pr_seq(o, opts) {
  if(function() {
    var and__3822__auto____16757 = o;
    if(and__3822__auto____16757) {
      return o.cljs$core$IPrintable$_pr_seq$arity$2
    }else {
      return and__3822__auto____16757
    }
  }()) {
    return o.cljs$core$IPrintable$_pr_seq$arity$2(o, opts)
  }else {
    var x__2431__auto____16758 = o == null ? null : o;
    return function() {
      var or__3824__auto____16759 = cljs.core._pr_seq[goog.typeOf(x__2431__auto____16758)];
      if(or__3824__auto____16759) {
        return or__3824__auto____16759
      }else {
        var or__3824__auto____16760 = cljs.core._pr_seq["_"];
        if(or__3824__auto____16760) {
          return or__3824__auto____16760
        }else {
          throw cljs.core.missing_protocol.call(null, "IPrintable.-pr-seq", o);
        }
      }
    }().call(null, o, opts)
  }
};
cljs.core.IWriter = {};
cljs.core._write = function _write(writer, s) {
  if(function() {
    var and__3822__auto____16765 = writer;
    if(and__3822__auto____16765) {
      return writer.cljs$core$IWriter$_write$arity$2
    }else {
      return and__3822__auto____16765
    }
  }()) {
    return writer.cljs$core$IWriter$_write$arity$2(writer, s)
  }else {
    var x__2431__auto____16766 = writer == null ? null : writer;
    return function() {
      var or__3824__auto____16767 = cljs.core._write[goog.typeOf(x__2431__auto____16766)];
      if(or__3824__auto____16767) {
        return or__3824__auto____16767
      }else {
        var or__3824__auto____16768 = cljs.core._write["_"];
        if(or__3824__auto____16768) {
          return or__3824__auto____16768
        }else {
          throw cljs.core.missing_protocol.call(null, "IWriter.-write", writer);
        }
      }
    }().call(null, writer, s)
  }
};
cljs.core._flush = function _flush(writer) {
  if(function() {
    var and__3822__auto____16773 = writer;
    if(and__3822__auto____16773) {
      return writer.cljs$core$IWriter$_flush$arity$1
    }else {
      return and__3822__auto____16773
    }
  }()) {
    return writer.cljs$core$IWriter$_flush$arity$1(writer)
  }else {
    var x__2431__auto____16774 = writer == null ? null : writer;
    return function() {
      var or__3824__auto____16775 = cljs.core._flush[goog.typeOf(x__2431__auto____16774)];
      if(or__3824__auto____16775) {
        return or__3824__auto____16775
      }else {
        var or__3824__auto____16776 = cljs.core._flush["_"];
        if(or__3824__auto____16776) {
          return or__3824__auto____16776
        }else {
          throw cljs.core.missing_protocol.call(null, "IWriter.-flush", writer);
        }
      }
    }().call(null, writer)
  }
};
cljs.core.IPrintWithWriter = {};
cljs.core._pr_writer = function _pr_writer(o, writer, opts) {
  if(function() {
    var and__3822__auto____16781 = o;
    if(and__3822__auto____16781) {
      return o.cljs$core$IPrintWithWriter$_pr_writer$arity$3
    }else {
      return and__3822__auto____16781
    }
  }()) {
    return o.cljs$core$IPrintWithWriter$_pr_writer$arity$3(o, writer, opts)
  }else {
    var x__2431__auto____16782 = o == null ? null : o;
    return function() {
      var or__3824__auto____16783 = cljs.core._pr_writer[goog.typeOf(x__2431__auto____16782)];
      if(or__3824__auto____16783) {
        return or__3824__auto____16783
      }else {
        var or__3824__auto____16784 = cljs.core._pr_writer["_"];
        if(or__3824__auto____16784) {
          return or__3824__auto____16784
        }else {
          throw cljs.core.missing_protocol.call(null, "IPrintWithWriter.-pr-writer", o);
        }
      }
    }().call(null, o, writer, opts)
  }
};
cljs.core.IPending = {};
cljs.core._realized_QMARK_ = function _realized_QMARK_(d) {
  if(function() {
    var and__3822__auto____16789 = d;
    if(and__3822__auto____16789) {
      return d.cljs$core$IPending$_realized_QMARK_$arity$1
    }else {
      return and__3822__auto____16789
    }
  }()) {
    return d.cljs$core$IPending$_realized_QMARK_$arity$1(d)
  }else {
    var x__2431__auto____16790 = d == null ? null : d;
    return function() {
      var or__3824__auto____16791 = cljs.core._realized_QMARK_[goog.typeOf(x__2431__auto____16790)];
      if(or__3824__auto____16791) {
        return or__3824__auto____16791
      }else {
        var or__3824__auto____16792 = cljs.core._realized_QMARK_["_"];
        if(or__3824__auto____16792) {
          return or__3824__auto____16792
        }else {
          throw cljs.core.missing_protocol.call(null, "IPending.-realized?", d);
        }
      }
    }().call(null, d)
  }
};
cljs.core.IWatchable = {};
cljs.core._notify_watches = function _notify_watches(this$, oldval, newval) {
  if(function() {
    var and__3822__auto____16797 = this$;
    if(and__3822__auto____16797) {
      return this$.cljs$core$IWatchable$_notify_watches$arity$3
    }else {
      return and__3822__auto____16797
    }
  }()) {
    return this$.cljs$core$IWatchable$_notify_watches$arity$3(this$, oldval, newval)
  }else {
    var x__2431__auto____16798 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____16799 = cljs.core._notify_watches[goog.typeOf(x__2431__auto____16798)];
      if(or__3824__auto____16799) {
        return or__3824__auto____16799
      }else {
        var or__3824__auto____16800 = cljs.core._notify_watches["_"];
        if(or__3824__auto____16800) {
          return or__3824__auto____16800
        }else {
          throw cljs.core.missing_protocol.call(null, "IWatchable.-notify-watches", this$);
        }
      }
    }().call(null, this$, oldval, newval)
  }
};
cljs.core._add_watch = function _add_watch(this$, key, f) {
  if(function() {
    var and__3822__auto____16805 = this$;
    if(and__3822__auto____16805) {
      return this$.cljs$core$IWatchable$_add_watch$arity$3
    }else {
      return and__3822__auto____16805
    }
  }()) {
    return this$.cljs$core$IWatchable$_add_watch$arity$3(this$, key, f)
  }else {
    var x__2431__auto____16806 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____16807 = cljs.core._add_watch[goog.typeOf(x__2431__auto____16806)];
      if(or__3824__auto____16807) {
        return or__3824__auto____16807
      }else {
        var or__3824__auto____16808 = cljs.core._add_watch["_"];
        if(or__3824__auto____16808) {
          return or__3824__auto____16808
        }else {
          throw cljs.core.missing_protocol.call(null, "IWatchable.-add-watch", this$);
        }
      }
    }().call(null, this$, key, f)
  }
};
cljs.core._remove_watch = function _remove_watch(this$, key) {
  if(function() {
    var and__3822__auto____16813 = this$;
    if(and__3822__auto____16813) {
      return this$.cljs$core$IWatchable$_remove_watch$arity$2
    }else {
      return and__3822__auto____16813
    }
  }()) {
    return this$.cljs$core$IWatchable$_remove_watch$arity$2(this$, key)
  }else {
    var x__2431__auto____16814 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____16815 = cljs.core._remove_watch[goog.typeOf(x__2431__auto____16814)];
      if(or__3824__auto____16815) {
        return or__3824__auto____16815
      }else {
        var or__3824__auto____16816 = cljs.core._remove_watch["_"];
        if(or__3824__auto____16816) {
          return or__3824__auto____16816
        }else {
          throw cljs.core.missing_protocol.call(null, "IWatchable.-remove-watch", this$);
        }
      }
    }().call(null, this$, key)
  }
};
cljs.core.IEditableCollection = {};
cljs.core._as_transient = function _as_transient(coll) {
  if(function() {
    var and__3822__auto____16821 = coll;
    if(and__3822__auto____16821) {
      return coll.cljs$core$IEditableCollection$_as_transient$arity$1
    }else {
      return and__3822__auto____16821
    }
  }()) {
    return coll.cljs$core$IEditableCollection$_as_transient$arity$1(coll)
  }else {
    var x__2431__auto____16822 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16823 = cljs.core._as_transient[goog.typeOf(x__2431__auto____16822)];
      if(or__3824__auto____16823) {
        return or__3824__auto____16823
      }else {
        var or__3824__auto____16824 = cljs.core._as_transient["_"];
        if(or__3824__auto____16824) {
          return or__3824__auto____16824
        }else {
          throw cljs.core.missing_protocol.call(null, "IEditableCollection.-as-transient", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.ITransientCollection = {};
cljs.core._conj_BANG_ = function _conj_BANG_(tcoll, val) {
  if(function() {
    var and__3822__auto____16829 = tcoll;
    if(and__3822__auto____16829) {
      return tcoll.cljs$core$ITransientCollection$_conj_BANG_$arity$2
    }else {
      return and__3822__auto____16829
    }
  }()) {
    return tcoll.cljs$core$ITransientCollection$_conj_BANG_$arity$2(tcoll, val)
  }else {
    var x__2431__auto____16830 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16831 = cljs.core._conj_BANG_[goog.typeOf(x__2431__auto____16830)];
      if(or__3824__auto____16831) {
        return or__3824__auto____16831
      }else {
        var or__3824__auto____16832 = cljs.core._conj_BANG_["_"];
        if(or__3824__auto____16832) {
          return or__3824__auto____16832
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientCollection.-conj!", tcoll);
        }
      }
    }().call(null, tcoll, val)
  }
};
cljs.core._persistent_BANG_ = function _persistent_BANG_(tcoll) {
  if(function() {
    var and__3822__auto____16837 = tcoll;
    if(and__3822__auto____16837) {
      return tcoll.cljs$core$ITransientCollection$_persistent_BANG_$arity$1
    }else {
      return and__3822__auto____16837
    }
  }()) {
    return tcoll.cljs$core$ITransientCollection$_persistent_BANG_$arity$1(tcoll)
  }else {
    var x__2431__auto____16838 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16839 = cljs.core._persistent_BANG_[goog.typeOf(x__2431__auto____16838)];
      if(or__3824__auto____16839) {
        return or__3824__auto____16839
      }else {
        var or__3824__auto____16840 = cljs.core._persistent_BANG_["_"];
        if(or__3824__auto____16840) {
          return or__3824__auto____16840
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientCollection.-persistent!", tcoll);
        }
      }
    }().call(null, tcoll)
  }
};
cljs.core.ITransientAssociative = {};
cljs.core._assoc_BANG_ = function _assoc_BANG_(tcoll, key, val) {
  if(function() {
    var and__3822__auto____16845 = tcoll;
    if(and__3822__auto____16845) {
      return tcoll.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3
    }else {
      return and__3822__auto____16845
    }
  }()) {
    return tcoll.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(tcoll, key, val)
  }else {
    var x__2431__auto____16846 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16847 = cljs.core._assoc_BANG_[goog.typeOf(x__2431__auto____16846)];
      if(or__3824__auto____16847) {
        return or__3824__auto____16847
      }else {
        var or__3824__auto____16848 = cljs.core._assoc_BANG_["_"];
        if(or__3824__auto____16848) {
          return or__3824__auto____16848
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientAssociative.-assoc!", tcoll);
        }
      }
    }().call(null, tcoll, key, val)
  }
};
cljs.core.ITransientMap = {};
cljs.core._dissoc_BANG_ = function _dissoc_BANG_(tcoll, key) {
  if(function() {
    var and__3822__auto____16853 = tcoll;
    if(and__3822__auto____16853) {
      return tcoll.cljs$core$ITransientMap$_dissoc_BANG_$arity$2
    }else {
      return and__3822__auto____16853
    }
  }()) {
    return tcoll.cljs$core$ITransientMap$_dissoc_BANG_$arity$2(tcoll, key)
  }else {
    var x__2431__auto____16854 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16855 = cljs.core._dissoc_BANG_[goog.typeOf(x__2431__auto____16854)];
      if(or__3824__auto____16855) {
        return or__3824__auto____16855
      }else {
        var or__3824__auto____16856 = cljs.core._dissoc_BANG_["_"];
        if(or__3824__auto____16856) {
          return or__3824__auto____16856
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientMap.-dissoc!", tcoll);
        }
      }
    }().call(null, tcoll, key)
  }
};
cljs.core.ITransientVector = {};
cljs.core._assoc_n_BANG_ = function _assoc_n_BANG_(tcoll, n, val) {
  if(function() {
    var and__3822__auto____16861 = tcoll;
    if(and__3822__auto____16861) {
      return tcoll.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3
    }else {
      return and__3822__auto____16861
    }
  }()) {
    return tcoll.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3(tcoll, n, val)
  }else {
    var x__2431__auto____16862 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16863 = cljs.core._assoc_n_BANG_[goog.typeOf(x__2431__auto____16862)];
      if(or__3824__auto____16863) {
        return or__3824__auto____16863
      }else {
        var or__3824__auto____16864 = cljs.core._assoc_n_BANG_["_"];
        if(or__3824__auto____16864) {
          return or__3824__auto____16864
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientVector.-assoc-n!", tcoll);
        }
      }
    }().call(null, tcoll, n, val)
  }
};
cljs.core._pop_BANG_ = function _pop_BANG_(tcoll) {
  if(function() {
    var and__3822__auto____16869 = tcoll;
    if(and__3822__auto____16869) {
      return tcoll.cljs$core$ITransientVector$_pop_BANG_$arity$1
    }else {
      return and__3822__auto____16869
    }
  }()) {
    return tcoll.cljs$core$ITransientVector$_pop_BANG_$arity$1(tcoll)
  }else {
    var x__2431__auto____16870 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16871 = cljs.core._pop_BANG_[goog.typeOf(x__2431__auto____16870)];
      if(or__3824__auto____16871) {
        return or__3824__auto____16871
      }else {
        var or__3824__auto____16872 = cljs.core._pop_BANG_["_"];
        if(or__3824__auto____16872) {
          return or__3824__auto____16872
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientVector.-pop!", tcoll);
        }
      }
    }().call(null, tcoll)
  }
};
cljs.core.ITransientSet = {};
cljs.core._disjoin_BANG_ = function _disjoin_BANG_(tcoll, v) {
  if(function() {
    var and__3822__auto____16877 = tcoll;
    if(and__3822__auto____16877) {
      return tcoll.cljs$core$ITransientSet$_disjoin_BANG_$arity$2
    }else {
      return and__3822__auto____16877
    }
  }()) {
    return tcoll.cljs$core$ITransientSet$_disjoin_BANG_$arity$2(tcoll, v)
  }else {
    var x__2431__auto____16878 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____16879 = cljs.core._disjoin_BANG_[goog.typeOf(x__2431__auto____16878)];
      if(or__3824__auto____16879) {
        return or__3824__auto____16879
      }else {
        var or__3824__auto____16880 = cljs.core._disjoin_BANG_["_"];
        if(or__3824__auto____16880) {
          return or__3824__auto____16880
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientSet.-disjoin!", tcoll);
        }
      }
    }().call(null, tcoll, v)
  }
};
cljs.core.IComparable = {};
cljs.core._compare = function _compare(x, y) {
  if(function() {
    var and__3822__auto____16885 = x;
    if(and__3822__auto____16885) {
      return x.cljs$core$IComparable$_compare$arity$2
    }else {
      return and__3822__auto____16885
    }
  }()) {
    return x.cljs$core$IComparable$_compare$arity$2(x, y)
  }else {
    var x__2431__auto____16886 = x == null ? null : x;
    return function() {
      var or__3824__auto____16887 = cljs.core._compare[goog.typeOf(x__2431__auto____16886)];
      if(or__3824__auto____16887) {
        return or__3824__auto____16887
      }else {
        var or__3824__auto____16888 = cljs.core._compare["_"];
        if(or__3824__auto____16888) {
          return or__3824__auto____16888
        }else {
          throw cljs.core.missing_protocol.call(null, "IComparable.-compare", x);
        }
      }
    }().call(null, x, y)
  }
};
cljs.core.IChunk = {};
cljs.core._drop_first = function _drop_first(coll) {
  if(function() {
    var and__3822__auto____16893 = coll;
    if(and__3822__auto____16893) {
      return coll.cljs$core$IChunk$_drop_first$arity$1
    }else {
      return and__3822__auto____16893
    }
  }()) {
    return coll.cljs$core$IChunk$_drop_first$arity$1(coll)
  }else {
    var x__2431__auto____16894 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16895 = cljs.core._drop_first[goog.typeOf(x__2431__auto____16894)];
      if(or__3824__auto____16895) {
        return or__3824__auto____16895
      }else {
        var or__3824__auto____16896 = cljs.core._drop_first["_"];
        if(or__3824__auto____16896) {
          return or__3824__auto____16896
        }else {
          throw cljs.core.missing_protocol.call(null, "IChunk.-drop-first", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.IChunkedSeq = {};
cljs.core._chunked_first = function _chunked_first(coll) {
  if(function() {
    var and__3822__auto____16901 = coll;
    if(and__3822__auto____16901) {
      return coll.cljs$core$IChunkedSeq$_chunked_first$arity$1
    }else {
      return and__3822__auto____16901
    }
  }()) {
    return coll.cljs$core$IChunkedSeq$_chunked_first$arity$1(coll)
  }else {
    var x__2431__auto____16902 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16903 = cljs.core._chunked_first[goog.typeOf(x__2431__auto____16902)];
      if(or__3824__auto____16903) {
        return or__3824__auto____16903
      }else {
        var or__3824__auto____16904 = cljs.core._chunked_first["_"];
        if(or__3824__auto____16904) {
          return or__3824__auto____16904
        }else {
          throw cljs.core.missing_protocol.call(null, "IChunkedSeq.-chunked-first", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._chunked_rest = function _chunked_rest(coll) {
  if(function() {
    var and__3822__auto____16909 = coll;
    if(and__3822__auto____16909) {
      return coll.cljs$core$IChunkedSeq$_chunked_rest$arity$1
    }else {
      return and__3822__auto____16909
    }
  }()) {
    return coll.cljs$core$IChunkedSeq$_chunked_rest$arity$1(coll)
  }else {
    var x__2431__auto____16910 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16911 = cljs.core._chunked_rest[goog.typeOf(x__2431__auto____16910)];
      if(or__3824__auto____16911) {
        return or__3824__auto____16911
      }else {
        var or__3824__auto____16912 = cljs.core._chunked_rest["_"];
        if(or__3824__auto____16912) {
          return or__3824__auto____16912
        }else {
          throw cljs.core.missing_protocol.call(null, "IChunkedSeq.-chunked-rest", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.IChunkedNext = {};
cljs.core._chunked_next = function _chunked_next(coll) {
  if(function() {
    var and__3822__auto____16917 = coll;
    if(and__3822__auto____16917) {
      return coll.cljs$core$IChunkedNext$_chunked_next$arity$1
    }else {
      return and__3822__auto____16917
    }
  }()) {
    return coll.cljs$core$IChunkedNext$_chunked_next$arity$1(coll)
  }else {
    var x__2431__auto____16918 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16919 = cljs.core._chunked_next[goog.typeOf(x__2431__auto____16918)];
      if(or__3824__auto____16919) {
        return or__3824__auto____16919
      }else {
        var or__3824__auto____16920 = cljs.core._chunked_next["_"];
        if(or__3824__auto____16920) {
          return or__3824__auto____16920
        }else {
          throw cljs.core.missing_protocol.call(null, "IChunkedNext.-chunked-next", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core.seq = function seq(coll) {
  if(coll == null) {
    return null
  }else {
    if(function() {
      var G__16924__16925 = coll;
      if(G__16924__16925) {
        if(function() {
          var or__3824__auto____16926 = G__16924__16925.cljs$lang$protocol_mask$partition0$ & 32;
          if(or__3824__auto____16926) {
            return or__3824__auto____16926
          }else {
            return G__16924__16925.cljs$core$ASeq$
          }
        }()) {
          return true
        }else {
          if(!G__16924__16925.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ASeq, G__16924__16925)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ASeq, G__16924__16925)
      }
    }()) {
      return coll
    }else {
      return cljs.core._seq.call(null, coll)
    }
  }
};
cljs.core.first = function first(coll) {
  if(coll == null) {
    return null
  }else {
    if(function() {
      var G__16931__16932 = coll;
      if(G__16931__16932) {
        if(function() {
          var or__3824__auto____16933 = G__16931__16932.cljs$lang$protocol_mask$partition0$ & 64;
          if(or__3824__auto____16933) {
            return or__3824__auto____16933
          }else {
            return G__16931__16932.cljs$core$ISeq$
          }
        }()) {
          return true
        }else {
          if(!G__16931__16932.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__16931__16932)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__16931__16932)
      }
    }()) {
      return cljs.core._first.call(null, coll)
    }else {
      var s__16934 = cljs.core.seq.call(null, coll);
      if(s__16934 == null) {
        return null
      }else {
        return cljs.core._first.call(null, s__16934)
      }
    }
  }
};
cljs.core.rest = function rest(coll) {
  if(!(coll == null)) {
    if(function() {
      var G__16939__16940 = coll;
      if(G__16939__16940) {
        if(function() {
          var or__3824__auto____16941 = G__16939__16940.cljs$lang$protocol_mask$partition0$ & 64;
          if(or__3824__auto____16941) {
            return or__3824__auto____16941
          }else {
            return G__16939__16940.cljs$core$ISeq$
          }
        }()) {
          return true
        }else {
          if(!G__16939__16940.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__16939__16940)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__16939__16940)
      }
    }()) {
      return cljs.core._rest.call(null, coll)
    }else {
      var s__16942 = cljs.core.seq.call(null, coll);
      if(!(s__16942 == null)) {
        return cljs.core._rest.call(null, s__16942)
      }else {
        return cljs.core.List.EMPTY
      }
    }
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.next = function next(coll) {
  if(coll == null) {
    return null
  }else {
    if(function() {
      var G__16946__16947 = coll;
      if(G__16946__16947) {
        if(function() {
          var or__3824__auto____16948 = G__16946__16947.cljs$lang$protocol_mask$partition0$ & 128;
          if(or__3824__auto____16948) {
            return or__3824__auto____16948
          }else {
            return G__16946__16947.cljs$core$INext$
          }
        }()) {
          return true
        }else {
          if(!G__16946__16947.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.INext, G__16946__16947)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.INext, G__16946__16947)
      }
    }()) {
      return cljs.core._next.call(null, coll)
    }else {
      return cljs.core.seq.call(null, cljs.core.rest.call(null, coll))
    }
  }
};
cljs.core._EQ_ = function() {
  var _EQ_ = null;
  var _EQ___1 = function(x) {
    return true
  };
  var _EQ___2 = function(x, y) {
    var or__3824__auto____16950 = x === y;
    if(or__3824__auto____16950) {
      return or__3824__auto____16950
    }else {
      return cljs.core._equiv.call(null, x, y)
    }
  };
  var _EQ___3 = function() {
    var G__16951__delegate = function(x, y, more) {
      while(true) {
        if(cljs.core.truth_(_EQ_.call(null, x, y))) {
          if(cljs.core.next.call(null, more)) {
            var G__16952 = y;
            var G__16953 = cljs.core.first.call(null, more);
            var G__16954 = cljs.core.next.call(null, more);
            x = G__16952;
            y = G__16953;
            more = G__16954;
            continue
          }else {
            return _EQ_.call(null, y, cljs.core.first.call(null, more))
          }
        }else {
          return false
        }
        break
      }
    };
    var G__16951 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__16951__delegate.call(this, x, y, more)
    };
    G__16951.cljs$lang$maxFixedArity = 2;
    G__16951.cljs$lang$applyTo = function(arglist__16955) {
      var x = cljs.core.first(arglist__16955);
      var y = cljs.core.first(cljs.core.next(arglist__16955));
      var more = cljs.core.rest(cljs.core.next(arglist__16955));
      return G__16951__delegate(x, y, more)
    };
    G__16951.cljs$lang$arity$variadic = G__16951__delegate;
    return G__16951
  }();
  _EQ_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _EQ___1.call(this, x);
      case 2:
        return _EQ___2.call(this, x, y);
      default:
        return _EQ___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _EQ_.cljs$lang$maxFixedArity = 2;
  _EQ_.cljs$lang$applyTo = _EQ___3.cljs$lang$applyTo;
  _EQ_.cljs$lang$arity$1 = _EQ___1;
  _EQ_.cljs$lang$arity$2 = _EQ___2;
  _EQ_.cljs$lang$arity$variadic = _EQ___3.cljs$lang$arity$variadic;
  return _EQ_
}();
cljs.core.type = function type(x) {
  if(x == null) {
    return null
  }else {
    return x.constructor
  }
};
cljs.core.instance_QMARK_ = function instance_QMARK_(t, o) {
  return o instanceof t
};
cljs.core.IHash["null"] = true;
cljs.core._hash["null"] = function(o) {
  return 0
};
cljs.core.ILookup["null"] = true;
cljs.core._lookup["null"] = function() {
  var G__16956 = null;
  var G__16956__2 = function(o, k) {
    return null
  };
  var G__16956__3 = function(o, k, not_found) {
    return not_found
  };
  G__16956 = function(o, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__16956__2.call(this, o, k);
      case 3:
        return G__16956__3.call(this, o, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__16956
}();
cljs.core.IAssociative["null"] = true;
cljs.core._assoc["null"] = function(_, k, v) {
  return cljs.core.hash_map.call(null, k, v)
};
cljs.core.INext["null"] = true;
cljs.core._next["null"] = function(_) {
  return null
};
cljs.core.IPrintWithWriter["null"] = true;
cljs.core._pr_writer["null"] = function(o, writer, _) {
  return cljs.core._write.call(null, writer, "nil")
};
cljs.core.ICollection["null"] = true;
cljs.core._conj["null"] = function(_, o) {
  return cljs.core.list.call(null, o)
};
cljs.core.IReduce["null"] = true;
cljs.core._reduce["null"] = function() {
  var G__16957 = null;
  var G__16957__2 = function(_, f) {
    return f.call(null)
  };
  var G__16957__3 = function(_, f, start) {
    return start
  };
  G__16957 = function(_, f, start) {
    switch(arguments.length) {
      case 2:
        return G__16957__2.call(this, _, f);
      case 3:
        return G__16957__3.call(this, _, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__16957
}();
cljs.core.IPrintable["null"] = true;
cljs.core._pr_seq["null"] = function(o) {
  return cljs.core.list.call(null, "nil")
};
cljs.core.ISet["null"] = true;
cljs.core._disjoin["null"] = function(_, v) {
  return null
};
cljs.core.ICounted["null"] = true;
cljs.core._count["null"] = function(_) {
  return 0
};
cljs.core.IStack["null"] = true;
cljs.core._peek["null"] = function(_) {
  return null
};
cljs.core._pop["null"] = function(_) {
  return null
};
cljs.core.ISeq["null"] = true;
cljs.core._first["null"] = function(_) {
  return null
};
cljs.core._rest["null"] = function(_) {
  return cljs.core.list.call(null)
};
cljs.core.IEquiv["null"] = true;
cljs.core._equiv["null"] = function(_, o) {
  return o == null
};
cljs.core.IWithMeta["null"] = true;
cljs.core._with_meta["null"] = function(_, meta) {
  return null
};
cljs.core.IMeta["null"] = true;
cljs.core._meta["null"] = function(_) {
  return null
};
cljs.core.IIndexed["null"] = true;
cljs.core._nth["null"] = function() {
  var G__16958 = null;
  var G__16958__2 = function(_, n) {
    return null
  };
  var G__16958__3 = function(_, n, not_found) {
    return not_found
  };
  G__16958 = function(_, n, not_found) {
    switch(arguments.length) {
      case 2:
        return G__16958__2.call(this, _, n);
      case 3:
        return G__16958__3.call(this, _, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__16958
}();
cljs.core.IEmptyableCollection["null"] = true;
cljs.core._empty["null"] = function(_) {
  return null
};
cljs.core.IMap["null"] = true;
cljs.core._dissoc["null"] = function(_, k) {
  return null
};
Date.prototype.cljs$core$IEquiv$ = true;
Date.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(o, other) {
  var and__3822__auto____16959 = cljs.core.instance_QMARK_.call(null, Date, other);
  if(and__3822__auto____16959) {
    return o.toString() === other.toString()
  }else {
    return and__3822__auto____16959
  }
};
cljs.core.IHash["number"] = true;
cljs.core._hash["number"] = function(o) {
  return o
};
cljs.core.IEquiv["number"] = true;
cljs.core._equiv["number"] = function(x, o) {
  return x === o
};
cljs.core.IHash["boolean"] = true;
cljs.core._hash["boolean"] = function(o) {
  if(o === true) {
    return 1
  }else {
    return 0
  }
};
cljs.core.IHash["_"] = true;
cljs.core._hash["_"] = function(o) {
  return goog.getUid(o)
};
cljs.core.inc = function inc(x) {
  return x + 1
};
goog.provide("cljs.core.Reduced");
cljs.core.Reduced = function(val) {
  this.val = val;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32768
};
cljs.core.Reduced.cljs$lang$type = true;
cljs.core.Reduced.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Reduced")
};
cljs.core.Reduced.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Reduced")
};
cljs.core.Reduced.prototype.cljs$core$IDeref$_deref$arity$1 = function(o) {
  var this__16960 = this;
  return this__16960.val
};
cljs.core.Reduced;
cljs.core.reduced = function reduced(x) {
  return new cljs.core.Reduced(x)
};
cljs.core.reduced_QMARK_ = function reduced_QMARK_(r) {
  return cljs.core.instance_QMARK_.call(null, cljs.core.Reduced, r)
};
cljs.core.ci_reduce = function() {
  var ci_reduce = null;
  var ci_reduce__2 = function(cicoll, f) {
    var cnt__16973 = cljs.core._count.call(null, cicoll);
    if(cnt__16973 === 0) {
      return f.call(null)
    }else {
      var val__16974 = cljs.core._nth.call(null, cicoll, 0);
      var n__16975 = 1;
      while(true) {
        if(n__16975 < cnt__16973) {
          var nval__16976 = f.call(null, val__16974, cljs.core._nth.call(null, cicoll, n__16975));
          if(cljs.core.reduced_QMARK_.call(null, nval__16976)) {
            return cljs.core.deref.call(null, nval__16976)
          }else {
            var G__16985 = nval__16976;
            var G__16986 = n__16975 + 1;
            val__16974 = G__16985;
            n__16975 = G__16986;
            continue
          }
        }else {
          return val__16974
        }
        break
      }
    }
  };
  var ci_reduce__3 = function(cicoll, f, val) {
    var cnt__16977 = cljs.core._count.call(null, cicoll);
    var val__16978 = val;
    var n__16979 = 0;
    while(true) {
      if(n__16979 < cnt__16977) {
        var nval__16980 = f.call(null, val__16978, cljs.core._nth.call(null, cicoll, n__16979));
        if(cljs.core.reduced_QMARK_.call(null, nval__16980)) {
          return cljs.core.deref.call(null, nval__16980)
        }else {
          var G__16987 = nval__16980;
          var G__16988 = n__16979 + 1;
          val__16978 = G__16987;
          n__16979 = G__16988;
          continue
        }
      }else {
        return val__16978
      }
      break
    }
  };
  var ci_reduce__4 = function(cicoll, f, val, idx) {
    var cnt__16981 = cljs.core._count.call(null, cicoll);
    var val__16982 = val;
    var n__16983 = idx;
    while(true) {
      if(n__16983 < cnt__16981) {
        var nval__16984 = f.call(null, val__16982, cljs.core._nth.call(null, cicoll, n__16983));
        if(cljs.core.reduced_QMARK_.call(null, nval__16984)) {
          return cljs.core.deref.call(null, nval__16984)
        }else {
          var G__16989 = nval__16984;
          var G__16990 = n__16983 + 1;
          val__16982 = G__16989;
          n__16983 = G__16990;
          continue
        }
      }else {
        return val__16982
      }
      break
    }
  };
  ci_reduce = function(cicoll, f, val, idx) {
    switch(arguments.length) {
      case 2:
        return ci_reduce__2.call(this, cicoll, f);
      case 3:
        return ci_reduce__3.call(this, cicoll, f, val);
      case 4:
        return ci_reduce__4.call(this, cicoll, f, val, idx)
    }
    throw"Invalid arity: " + arguments.length;
  };
  ci_reduce.cljs$lang$arity$2 = ci_reduce__2;
  ci_reduce.cljs$lang$arity$3 = ci_reduce__3;
  ci_reduce.cljs$lang$arity$4 = ci_reduce__4;
  return ci_reduce
}();
cljs.core.array_reduce = function() {
  var array_reduce = null;
  var array_reduce__2 = function(arr, f) {
    var cnt__17003 = arr.length;
    if(arr.length === 0) {
      return f.call(null)
    }else {
      var val__17004 = arr[0];
      var n__17005 = 1;
      while(true) {
        if(n__17005 < cnt__17003) {
          var nval__17006 = f.call(null, val__17004, arr[n__17005]);
          if(cljs.core.reduced_QMARK_.call(null, nval__17006)) {
            return cljs.core.deref.call(null, nval__17006)
          }else {
            var G__17015 = nval__17006;
            var G__17016 = n__17005 + 1;
            val__17004 = G__17015;
            n__17005 = G__17016;
            continue
          }
        }else {
          return val__17004
        }
        break
      }
    }
  };
  var array_reduce__3 = function(arr, f, val) {
    var cnt__17007 = arr.length;
    var val__17008 = val;
    var n__17009 = 0;
    while(true) {
      if(n__17009 < cnt__17007) {
        var nval__17010 = f.call(null, val__17008, arr[n__17009]);
        if(cljs.core.reduced_QMARK_.call(null, nval__17010)) {
          return cljs.core.deref.call(null, nval__17010)
        }else {
          var G__17017 = nval__17010;
          var G__17018 = n__17009 + 1;
          val__17008 = G__17017;
          n__17009 = G__17018;
          continue
        }
      }else {
        return val__17008
      }
      break
    }
  };
  var array_reduce__4 = function(arr, f, val, idx) {
    var cnt__17011 = arr.length;
    var val__17012 = val;
    var n__17013 = idx;
    while(true) {
      if(n__17013 < cnt__17011) {
        var nval__17014 = f.call(null, val__17012, arr[n__17013]);
        if(cljs.core.reduced_QMARK_.call(null, nval__17014)) {
          return cljs.core.deref.call(null, nval__17014)
        }else {
          var G__17019 = nval__17014;
          var G__17020 = n__17013 + 1;
          val__17012 = G__17019;
          n__17013 = G__17020;
          continue
        }
      }else {
        return val__17012
      }
      break
    }
  };
  array_reduce = function(arr, f, val, idx) {
    switch(arguments.length) {
      case 2:
        return array_reduce__2.call(this, arr, f);
      case 3:
        return array_reduce__3.call(this, arr, f, val);
      case 4:
        return array_reduce__4.call(this, arr, f, val, idx)
    }
    throw"Invalid arity: " + arguments.length;
  };
  array_reduce.cljs$lang$arity$2 = array_reduce__2;
  array_reduce.cljs$lang$arity$3 = array_reduce__3;
  array_reduce.cljs$lang$arity$4 = array_reduce__4;
  return array_reduce
}();
cljs.core.counted_QMARK_ = function counted_QMARK_(x) {
  var G__17024__17025 = x;
  if(G__17024__17025) {
    if(function() {
      var or__3824__auto____17026 = G__17024__17025.cljs$lang$protocol_mask$partition0$ & 2;
      if(or__3824__auto____17026) {
        return or__3824__auto____17026
      }else {
        return G__17024__17025.cljs$core$ICounted$
      }
    }()) {
      return true
    }else {
      if(!G__17024__17025.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.ICounted, G__17024__17025)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.ICounted, G__17024__17025)
  }
};
cljs.core.indexed_QMARK_ = function indexed_QMARK_(x) {
  var G__17030__17031 = x;
  if(G__17030__17031) {
    if(function() {
      var or__3824__auto____17032 = G__17030__17031.cljs$lang$protocol_mask$partition0$ & 16;
      if(or__3824__auto____17032) {
        return or__3824__auto____17032
      }else {
        return G__17030__17031.cljs$core$IIndexed$
      }
    }()) {
      return true
    }else {
      if(!G__17030__17031.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17030__17031)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17030__17031)
  }
};
goog.provide("cljs.core.IndexedSeq");
cljs.core.IndexedSeq = function(a, i) {
  this.a = a;
  this.i = i;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 166199550
};
cljs.core.IndexedSeq.cljs$lang$type = true;
cljs.core.IndexedSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/IndexedSeq")
};
cljs.core.IndexedSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/IndexedSeq")
};
cljs.core.IndexedSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17033 = this;
  return cljs.core.hash_coll.call(null, coll)
};
cljs.core.IndexedSeq.prototype.cljs$core$INext$_next$arity$1 = function(_) {
  var this__17034 = this;
  if(this__17034.i + 1 < this__17034.a.length) {
    return new cljs.core.IndexedSeq(this__17034.a, this__17034.i + 1)
  }else {
    return null
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17035 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.IndexedSeq.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__17036 = this;
  var c__17037 = coll.cljs$core$ICounted$_count$arity$1(coll);
  if(c__17037 > 0) {
    return new cljs.core.RSeq(coll, c__17037 - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.IndexedSeq.prototype.toString = function() {
  var this__17038 = this;
  var this__17039 = this;
  return cljs.core.pr_str.call(null, this__17039)
};
cljs.core.IndexedSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(coll, f) {
  var this__17040 = this;
  if(cljs.core.counted_QMARK_.call(null, this__17040.a)) {
    return cljs.core.ci_reduce.call(null, this__17040.a, f, this__17040.a[this__17040.i], this__17040.i + 1)
  }else {
    return cljs.core.ci_reduce.call(null, coll, f, this__17040.a[this__17040.i], 0)
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(coll, f, start) {
  var this__17041 = this;
  if(cljs.core.counted_QMARK_.call(null, this__17041.a)) {
    return cljs.core.ci_reduce.call(null, this__17041.a, f, start, this__17041.i)
  }else {
    return cljs.core.ci_reduce.call(null, coll, f, start, 0)
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__17042 = this;
  return this$
};
cljs.core.IndexedSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(_) {
  var this__17043 = this;
  return this__17043.a.length - this__17043.i
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(_) {
  var this__17044 = this;
  return this__17044.a[this__17044.i]
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(_) {
  var this__17045 = this;
  if(this__17045.i + 1 < this__17045.a.length) {
    return new cljs.core.IndexedSeq(this__17045.a, this__17045.i + 1)
  }else {
    return cljs.core.list.call(null)
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17046 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.IndexedSeq.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__17047 = this;
  var i__17048 = n + this__17047.i;
  if(i__17048 < this__17047.a.length) {
    return this__17047.a[i__17048]
  }else {
    return null
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__17049 = this;
  var i__17050 = n + this__17049.i;
  if(i__17050 < this__17049.a.length) {
    return this__17049.a[i__17050]
  }else {
    return not_found
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17051 = this;
  return cljs.core.List.EMPTY
};
cljs.core.IndexedSeq;
cljs.core.prim_seq = function() {
  var prim_seq = null;
  var prim_seq__1 = function(prim) {
    return prim_seq.call(null, prim, 0)
  };
  var prim_seq__2 = function(prim, i) {
    if(prim.length === 0) {
      return null
    }else {
      return new cljs.core.IndexedSeq(prim, i)
    }
  };
  prim_seq = function(prim, i) {
    switch(arguments.length) {
      case 1:
        return prim_seq__1.call(this, prim);
      case 2:
        return prim_seq__2.call(this, prim, i)
    }
    throw"Invalid arity: " + arguments.length;
  };
  prim_seq.cljs$lang$arity$1 = prim_seq__1;
  prim_seq.cljs$lang$arity$2 = prim_seq__2;
  return prim_seq
}();
cljs.core.array_seq = function() {
  var array_seq = null;
  var array_seq__1 = function(array) {
    return cljs.core.prim_seq.call(null, array, 0)
  };
  var array_seq__2 = function(array, i) {
    return cljs.core.prim_seq.call(null, array, i)
  };
  array_seq = function(array, i) {
    switch(arguments.length) {
      case 1:
        return array_seq__1.call(this, array);
      case 2:
        return array_seq__2.call(this, array, i)
    }
    throw"Invalid arity: " + arguments.length;
  };
  array_seq.cljs$lang$arity$1 = array_seq__1;
  array_seq.cljs$lang$arity$2 = array_seq__2;
  return array_seq
}();
cljs.core.IReduce["array"] = true;
cljs.core._reduce["array"] = function() {
  var G__17052 = null;
  var G__17052__2 = function(array, f) {
    return cljs.core.ci_reduce.call(null, array, f)
  };
  var G__17052__3 = function(array, f, start) {
    return cljs.core.ci_reduce.call(null, array, f, start)
  };
  G__17052 = function(array, f, start) {
    switch(arguments.length) {
      case 2:
        return G__17052__2.call(this, array, f);
      case 3:
        return G__17052__3.call(this, array, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17052
}();
cljs.core.ILookup["array"] = true;
cljs.core._lookup["array"] = function() {
  var G__17053 = null;
  var G__17053__2 = function(array, k) {
    return array[k]
  };
  var G__17053__3 = function(array, k, not_found) {
    return cljs.core._nth.call(null, array, k, not_found)
  };
  G__17053 = function(array, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17053__2.call(this, array, k);
      case 3:
        return G__17053__3.call(this, array, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17053
}();
cljs.core.IIndexed["array"] = true;
cljs.core._nth["array"] = function() {
  var G__17054 = null;
  var G__17054__2 = function(array, n) {
    if(n < array.length) {
      return array[n]
    }else {
      return null
    }
  };
  var G__17054__3 = function(array, n, not_found) {
    if(n < array.length) {
      return array[n]
    }else {
      return not_found
    }
  };
  G__17054 = function(array, n, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17054__2.call(this, array, n);
      case 3:
        return G__17054__3.call(this, array, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17054
}();
cljs.core.ICounted["array"] = true;
cljs.core._count["array"] = function(a) {
  return a.length
};
cljs.core.ISeqable["array"] = true;
cljs.core._seq["array"] = function(array) {
  return cljs.core.array_seq.call(null, array, 0)
};
goog.provide("cljs.core.RSeq");
cljs.core.RSeq = function(ci, i, meta) {
  this.ci = ci;
  this.i = i;
  this.meta = meta;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850574
};
cljs.core.RSeq.cljs$lang$type = true;
cljs.core.RSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/RSeq")
};
cljs.core.RSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/RSeq")
};
cljs.core.RSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17055 = this;
  return cljs.core.hash_coll.call(null, coll)
};
cljs.core.RSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17056 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.RSeq.prototype.toString = function() {
  var this__17057 = this;
  var this__17058 = this;
  return cljs.core.pr_str.call(null, this__17058)
};
cljs.core.RSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17059 = this;
  return coll
};
cljs.core.RSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__17060 = this;
  return this__17060.i + 1
};
cljs.core.RSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17061 = this;
  return cljs.core._nth.call(null, this__17061.ci, this__17061.i)
};
cljs.core.RSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17062 = this;
  if(this__17062.i > 0) {
    return new cljs.core.RSeq(this__17062.ci, this__17062.i - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.RSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17063 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.RSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, new_meta) {
  var this__17064 = this;
  return new cljs.core.RSeq(this__17064.ci, this__17064.i, new_meta)
};
cljs.core.RSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17065 = this;
  return this__17065.meta
};
cljs.core.RSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17066 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17066.meta)
};
cljs.core.RSeq;
cljs.core.second = function second(coll) {
  return cljs.core.first.call(null, cljs.core.next.call(null, coll))
};
cljs.core.ffirst = function ffirst(coll) {
  return cljs.core.first.call(null, cljs.core.first.call(null, coll))
};
cljs.core.nfirst = function nfirst(coll) {
  return cljs.core.next.call(null, cljs.core.first.call(null, coll))
};
cljs.core.fnext = function fnext(coll) {
  return cljs.core.first.call(null, cljs.core.next.call(null, coll))
};
cljs.core.nnext = function nnext(coll) {
  return cljs.core.next.call(null, cljs.core.next.call(null, coll))
};
cljs.core.last = function last(s) {
  while(true) {
    var sn__17068 = cljs.core.next.call(null, s);
    if(!(sn__17068 == null)) {
      var G__17069 = sn__17068;
      s = G__17069;
      continue
    }else {
      return cljs.core.first.call(null, s)
    }
    break
  }
};
cljs.core.IEquiv["_"] = true;
cljs.core._equiv["_"] = function(x, o) {
  return x === o
};
cljs.core.conj = function() {
  var conj = null;
  var conj__2 = function(coll, x) {
    return cljs.core._conj.call(null, coll, x)
  };
  var conj__3 = function() {
    var G__17070__delegate = function(coll, x, xs) {
      while(true) {
        if(cljs.core.truth_(xs)) {
          var G__17071 = conj.call(null, coll, x);
          var G__17072 = cljs.core.first.call(null, xs);
          var G__17073 = cljs.core.next.call(null, xs);
          coll = G__17071;
          x = G__17072;
          xs = G__17073;
          continue
        }else {
          return conj.call(null, coll, x)
        }
        break
      }
    };
    var G__17070 = function(coll, x, var_args) {
      var xs = null;
      if(goog.isDef(var_args)) {
        xs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17070__delegate.call(this, coll, x, xs)
    };
    G__17070.cljs$lang$maxFixedArity = 2;
    G__17070.cljs$lang$applyTo = function(arglist__17074) {
      var coll = cljs.core.first(arglist__17074);
      var x = cljs.core.first(cljs.core.next(arglist__17074));
      var xs = cljs.core.rest(cljs.core.next(arglist__17074));
      return G__17070__delegate(coll, x, xs)
    };
    G__17070.cljs$lang$arity$variadic = G__17070__delegate;
    return G__17070
  }();
  conj = function(coll, x, var_args) {
    var xs = var_args;
    switch(arguments.length) {
      case 2:
        return conj__2.call(this, coll, x);
      default:
        return conj__3.cljs$lang$arity$variadic(coll, x, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  conj.cljs$lang$maxFixedArity = 2;
  conj.cljs$lang$applyTo = conj__3.cljs$lang$applyTo;
  conj.cljs$lang$arity$2 = conj__2;
  conj.cljs$lang$arity$variadic = conj__3.cljs$lang$arity$variadic;
  return conj
}();
cljs.core.empty = function empty(coll) {
  return cljs.core._empty.call(null, coll)
};
cljs.core.accumulating_seq_count = function accumulating_seq_count(coll) {
  var s__17077 = cljs.core.seq.call(null, coll);
  var acc__17078 = 0;
  while(true) {
    if(cljs.core.counted_QMARK_.call(null, s__17077)) {
      return acc__17078 + cljs.core._count.call(null, s__17077)
    }else {
      var G__17079 = cljs.core.next.call(null, s__17077);
      var G__17080 = acc__17078 + 1;
      s__17077 = G__17079;
      acc__17078 = G__17080;
      continue
    }
    break
  }
};
cljs.core.count = function count(coll) {
  if(cljs.core.counted_QMARK_.call(null, coll)) {
    return cljs.core._count.call(null, coll)
  }else {
    return cljs.core.accumulating_seq_count.call(null, coll)
  }
};
cljs.core.linear_traversal_nth = function() {
  var linear_traversal_nth = null;
  var linear_traversal_nth__2 = function(coll, n) {
    while(true) {
      if(coll == null) {
        throw new Error("Index out of bounds");
      }else {
        if(n === 0) {
          if(cljs.core.seq.call(null, coll)) {
            return cljs.core.first.call(null, coll)
          }else {
            throw new Error("Index out of bounds");
          }
        }else {
          if(cljs.core.indexed_QMARK_.call(null, coll)) {
            return cljs.core._nth.call(null, coll, n)
          }else {
            if(cljs.core.seq.call(null, coll)) {
              var G__17081 = cljs.core.next.call(null, coll);
              var G__17082 = n - 1;
              coll = G__17081;
              n = G__17082;
              continue
            }else {
              if("\ufdd0'else") {
                throw new Error("Index out of bounds");
              }else {
                return null
              }
            }
          }
        }
      }
      break
    }
  };
  var linear_traversal_nth__3 = function(coll, n, not_found) {
    while(true) {
      if(coll == null) {
        return not_found
      }else {
        if(n === 0) {
          if(cljs.core.seq.call(null, coll)) {
            return cljs.core.first.call(null, coll)
          }else {
            return not_found
          }
        }else {
          if(cljs.core.indexed_QMARK_.call(null, coll)) {
            return cljs.core._nth.call(null, coll, n, not_found)
          }else {
            if(cljs.core.seq.call(null, coll)) {
              var G__17083 = cljs.core.next.call(null, coll);
              var G__17084 = n - 1;
              var G__17085 = not_found;
              coll = G__17083;
              n = G__17084;
              not_found = G__17085;
              continue
            }else {
              if("\ufdd0'else") {
                return not_found
              }else {
                return null
              }
            }
          }
        }
      }
      break
    }
  };
  linear_traversal_nth = function(coll, n, not_found) {
    switch(arguments.length) {
      case 2:
        return linear_traversal_nth__2.call(this, coll, n);
      case 3:
        return linear_traversal_nth__3.call(this, coll, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  linear_traversal_nth.cljs$lang$arity$2 = linear_traversal_nth__2;
  linear_traversal_nth.cljs$lang$arity$3 = linear_traversal_nth__3;
  return linear_traversal_nth
}();
cljs.core.nth = function() {
  var nth = null;
  var nth__2 = function(coll, n) {
    if(coll == null) {
      return null
    }else {
      if(function() {
        var G__17092__17093 = coll;
        if(G__17092__17093) {
          if(function() {
            var or__3824__auto____17094 = G__17092__17093.cljs$lang$protocol_mask$partition0$ & 16;
            if(or__3824__auto____17094) {
              return or__3824__auto____17094
            }else {
              return G__17092__17093.cljs$core$IIndexed$
            }
          }()) {
            return true
          }else {
            if(!G__17092__17093.cljs$lang$protocol_mask$partition0$) {
              return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17092__17093)
            }else {
              return false
            }
          }
        }else {
          return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17092__17093)
        }
      }()) {
        return cljs.core._nth.call(null, coll, Math.floor(n))
      }else {
        return cljs.core.linear_traversal_nth.call(null, coll, Math.floor(n))
      }
    }
  };
  var nth__3 = function(coll, n, not_found) {
    if(!(coll == null)) {
      if(function() {
        var G__17095__17096 = coll;
        if(G__17095__17096) {
          if(function() {
            var or__3824__auto____17097 = G__17095__17096.cljs$lang$protocol_mask$partition0$ & 16;
            if(or__3824__auto____17097) {
              return or__3824__auto____17097
            }else {
              return G__17095__17096.cljs$core$IIndexed$
            }
          }()) {
            return true
          }else {
            if(!G__17095__17096.cljs$lang$protocol_mask$partition0$) {
              return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17095__17096)
            }else {
              return false
            }
          }
        }else {
          return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17095__17096)
        }
      }()) {
        return cljs.core._nth.call(null, coll, Math.floor(n), not_found)
      }else {
        return cljs.core.linear_traversal_nth.call(null, coll, Math.floor(n), not_found)
      }
    }else {
      return not_found
    }
  };
  nth = function(coll, n, not_found) {
    switch(arguments.length) {
      case 2:
        return nth__2.call(this, coll, n);
      case 3:
        return nth__3.call(this, coll, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  nth.cljs$lang$arity$2 = nth__2;
  nth.cljs$lang$arity$3 = nth__3;
  return nth
}();
cljs.core.get = function() {
  var get = null;
  var get__2 = function(o, k) {
    return cljs.core._lookup.call(null, o, k)
  };
  var get__3 = function(o, k, not_found) {
    return cljs.core._lookup.call(null, o, k, not_found)
  };
  get = function(o, k, not_found) {
    switch(arguments.length) {
      case 2:
        return get__2.call(this, o, k);
      case 3:
        return get__3.call(this, o, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  get.cljs$lang$arity$2 = get__2;
  get.cljs$lang$arity$3 = get__3;
  return get
}();
cljs.core.assoc = function() {
  var assoc = null;
  var assoc__3 = function(coll, k, v) {
    return cljs.core._assoc.call(null, coll, k, v)
  };
  var assoc__4 = function() {
    var G__17100__delegate = function(coll, k, v, kvs) {
      while(true) {
        var ret__17099 = assoc.call(null, coll, k, v);
        if(cljs.core.truth_(kvs)) {
          var G__17101 = ret__17099;
          var G__17102 = cljs.core.first.call(null, kvs);
          var G__17103 = cljs.core.second.call(null, kvs);
          var G__17104 = cljs.core.nnext.call(null, kvs);
          coll = G__17101;
          k = G__17102;
          v = G__17103;
          kvs = G__17104;
          continue
        }else {
          return ret__17099
        }
        break
      }
    };
    var G__17100 = function(coll, k, v, var_args) {
      var kvs = null;
      if(goog.isDef(var_args)) {
        kvs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__17100__delegate.call(this, coll, k, v, kvs)
    };
    G__17100.cljs$lang$maxFixedArity = 3;
    G__17100.cljs$lang$applyTo = function(arglist__17105) {
      var coll = cljs.core.first(arglist__17105);
      var k = cljs.core.first(cljs.core.next(arglist__17105));
      var v = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17105)));
      var kvs = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17105)));
      return G__17100__delegate(coll, k, v, kvs)
    };
    G__17100.cljs$lang$arity$variadic = G__17100__delegate;
    return G__17100
  }();
  assoc = function(coll, k, v, var_args) {
    var kvs = var_args;
    switch(arguments.length) {
      case 3:
        return assoc__3.call(this, coll, k, v);
      default:
        return assoc__4.cljs$lang$arity$variadic(coll, k, v, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  assoc.cljs$lang$maxFixedArity = 3;
  assoc.cljs$lang$applyTo = assoc__4.cljs$lang$applyTo;
  assoc.cljs$lang$arity$3 = assoc__3;
  assoc.cljs$lang$arity$variadic = assoc__4.cljs$lang$arity$variadic;
  return assoc
}();
cljs.core.dissoc = function() {
  var dissoc = null;
  var dissoc__1 = function(coll) {
    return coll
  };
  var dissoc__2 = function(coll, k) {
    return cljs.core._dissoc.call(null, coll, k)
  };
  var dissoc__3 = function() {
    var G__17108__delegate = function(coll, k, ks) {
      while(true) {
        var ret__17107 = dissoc.call(null, coll, k);
        if(cljs.core.truth_(ks)) {
          var G__17109 = ret__17107;
          var G__17110 = cljs.core.first.call(null, ks);
          var G__17111 = cljs.core.next.call(null, ks);
          coll = G__17109;
          k = G__17110;
          ks = G__17111;
          continue
        }else {
          return ret__17107
        }
        break
      }
    };
    var G__17108 = function(coll, k, var_args) {
      var ks = null;
      if(goog.isDef(var_args)) {
        ks = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17108__delegate.call(this, coll, k, ks)
    };
    G__17108.cljs$lang$maxFixedArity = 2;
    G__17108.cljs$lang$applyTo = function(arglist__17112) {
      var coll = cljs.core.first(arglist__17112);
      var k = cljs.core.first(cljs.core.next(arglist__17112));
      var ks = cljs.core.rest(cljs.core.next(arglist__17112));
      return G__17108__delegate(coll, k, ks)
    };
    G__17108.cljs$lang$arity$variadic = G__17108__delegate;
    return G__17108
  }();
  dissoc = function(coll, k, var_args) {
    var ks = var_args;
    switch(arguments.length) {
      case 1:
        return dissoc__1.call(this, coll);
      case 2:
        return dissoc__2.call(this, coll, k);
      default:
        return dissoc__3.cljs$lang$arity$variadic(coll, k, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  dissoc.cljs$lang$maxFixedArity = 2;
  dissoc.cljs$lang$applyTo = dissoc__3.cljs$lang$applyTo;
  dissoc.cljs$lang$arity$1 = dissoc__1;
  dissoc.cljs$lang$arity$2 = dissoc__2;
  dissoc.cljs$lang$arity$variadic = dissoc__3.cljs$lang$arity$variadic;
  return dissoc
}();
cljs.core.with_meta = function with_meta(o, meta) {
  return cljs.core._with_meta.call(null, o, meta)
};
cljs.core.meta = function meta(o) {
  if(function() {
    var G__17116__17117 = o;
    if(G__17116__17117) {
      if(function() {
        var or__3824__auto____17118 = G__17116__17117.cljs$lang$protocol_mask$partition0$ & 131072;
        if(or__3824__auto____17118) {
          return or__3824__auto____17118
        }else {
          return G__17116__17117.cljs$core$IMeta$
        }
      }()) {
        return true
      }else {
        if(!G__17116__17117.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__17116__17117)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__17116__17117)
    }
  }()) {
    return cljs.core._meta.call(null, o)
  }else {
    return null
  }
};
cljs.core.peek = function peek(coll) {
  return cljs.core._peek.call(null, coll)
};
cljs.core.pop = function pop(coll) {
  return cljs.core._pop.call(null, coll)
};
cljs.core.disj = function() {
  var disj = null;
  var disj__1 = function(coll) {
    return coll
  };
  var disj__2 = function(coll, k) {
    return cljs.core._disjoin.call(null, coll, k)
  };
  var disj__3 = function() {
    var G__17121__delegate = function(coll, k, ks) {
      while(true) {
        var ret__17120 = disj.call(null, coll, k);
        if(cljs.core.truth_(ks)) {
          var G__17122 = ret__17120;
          var G__17123 = cljs.core.first.call(null, ks);
          var G__17124 = cljs.core.next.call(null, ks);
          coll = G__17122;
          k = G__17123;
          ks = G__17124;
          continue
        }else {
          return ret__17120
        }
        break
      }
    };
    var G__17121 = function(coll, k, var_args) {
      var ks = null;
      if(goog.isDef(var_args)) {
        ks = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17121__delegate.call(this, coll, k, ks)
    };
    G__17121.cljs$lang$maxFixedArity = 2;
    G__17121.cljs$lang$applyTo = function(arglist__17125) {
      var coll = cljs.core.first(arglist__17125);
      var k = cljs.core.first(cljs.core.next(arglist__17125));
      var ks = cljs.core.rest(cljs.core.next(arglist__17125));
      return G__17121__delegate(coll, k, ks)
    };
    G__17121.cljs$lang$arity$variadic = G__17121__delegate;
    return G__17121
  }();
  disj = function(coll, k, var_args) {
    var ks = var_args;
    switch(arguments.length) {
      case 1:
        return disj__1.call(this, coll);
      case 2:
        return disj__2.call(this, coll, k);
      default:
        return disj__3.cljs$lang$arity$variadic(coll, k, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  disj.cljs$lang$maxFixedArity = 2;
  disj.cljs$lang$applyTo = disj__3.cljs$lang$applyTo;
  disj.cljs$lang$arity$1 = disj__1;
  disj.cljs$lang$arity$2 = disj__2;
  disj.cljs$lang$arity$variadic = disj__3.cljs$lang$arity$variadic;
  return disj
}();
cljs.core.string_hash_cache = {};
cljs.core.string_hash_cache_count = 0;
cljs.core.add_to_string_hash_cache = function add_to_string_hash_cache(k) {
  var h__17127 = goog.string.hashCode(k);
  cljs.core.string_hash_cache[k] = h__17127;
  cljs.core.string_hash_cache_count = cljs.core.string_hash_cache_count + 1;
  return h__17127
};
cljs.core.check_string_hash_cache = function check_string_hash_cache(k) {
  if(cljs.core.string_hash_cache_count > 255) {
    cljs.core.string_hash_cache = {};
    cljs.core.string_hash_cache_count = 0
  }else {
  }
  var h__17129 = cljs.core.string_hash_cache[k];
  if(!(h__17129 == null)) {
    return h__17129
  }else {
    return cljs.core.add_to_string_hash_cache.call(null, k)
  }
};
cljs.core.hash = function() {
  var hash = null;
  var hash__1 = function(o) {
    return hash.call(null, o, true)
  };
  var hash__2 = function(o, check_cache) {
    if(function() {
      var and__3822__auto____17131 = goog.isString(o);
      if(and__3822__auto____17131) {
        return check_cache
      }else {
        return and__3822__auto____17131
      }
    }()) {
      return cljs.core.check_string_hash_cache.call(null, o)
    }else {
      return cljs.core._hash.call(null, o)
    }
  };
  hash = function(o, check_cache) {
    switch(arguments.length) {
      case 1:
        return hash__1.call(this, o);
      case 2:
        return hash__2.call(this, o, check_cache)
    }
    throw"Invalid arity: " + arguments.length;
  };
  hash.cljs$lang$arity$1 = hash__1;
  hash.cljs$lang$arity$2 = hash__2;
  return hash
}();
cljs.core.empty_QMARK_ = function empty_QMARK_(coll) {
  return cljs.core.not.call(null, cljs.core.seq.call(null, coll))
};
cljs.core.coll_QMARK_ = function coll_QMARK_(x) {
  if(x == null) {
    return false
  }else {
    var G__17135__17136 = x;
    if(G__17135__17136) {
      if(function() {
        var or__3824__auto____17137 = G__17135__17136.cljs$lang$protocol_mask$partition0$ & 8;
        if(or__3824__auto____17137) {
          return or__3824__auto____17137
        }else {
          return G__17135__17136.cljs$core$ICollection$
        }
      }()) {
        return true
      }else {
        if(!G__17135__17136.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.ICollection, G__17135__17136)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.ICollection, G__17135__17136)
    }
  }
};
cljs.core.set_QMARK_ = function set_QMARK_(x) {
  if(x == null) {
    return false
  }else {
    var G__17141__17142 = x;
    if(G__17141__17142) {
      if(function() {
        var or__3824__auto____17143 = G__17141__17142.cljs$lang$protocol_mask$partition0$ & 4096;
        if(or__3824__auto____17143) {
          return or__3824__auto____17143
        }else {
          return G__17141__17142.cljs$core$ISet$
        }
      }()) {
        return true
      }else {
        if(!G__17141__17142.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.ISet, G__17141__17142)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.ISet, G__17141__17142)
    }
  }
};
cljs.core.associative_QMARK_ = function associative_QMARK_(x) {
  var G__17147__17148 = x;
  if(G__17147__17148) {
    if(function() {
      var or__3824__auto____17149 = G__17147__17148.cljs$lang$protocol_mask$partition0$ & 512;
      if(or__3824__auto____17149) {
        return or__3824__auto____17149
      }else {
        return G__17147__17148.cljs$core$IAssociative$
      }
    }()) {
      return true
    }else {
      if(!G__17147__17148.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IAssociative, G__17147__17148)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IAssociative, G__17147__17148)
  }
};
cljs.core.sequential_QMARK_ = function sequential_QMARK_(x) {
  var G__17153__17154 = x;
  if(G__17153__17154) {
    if(function() {
      var or__3824__auto____17155 = G__17153__17154.cljs$lang$protocol_mask$partition0$ & 16777216;
      if(or__3824__auto____17155) {
        return or__3824__auto____17155
      }else {
        return G__17153__17154.cljs$core$ISequential$
      }
    }()) {
      return true
    }else {
      if(!G__17153__17154.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISequential, G__17153__17154)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.ISequential, G__17153__17154)
  }
};
cljs.core.reduceable_QMARK_ = function reduceable_QMARK_(x) {
  var G__17159__17160 = x;
  if(G__17159__17160) {
    if(function() {
      var or__3824__auto____17161 = G__17159__17160.cljs$lang$protocol_mask$partition0$ & 524288;
      if(or__3824__auto____17161) {
        return or__3824__auto____17161
      }else {
        return G__17159__17160.cljs$core$IReduce$
      }
    }()) {
      return true
    }else {
      if(!G__17159__17160.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17159__17160)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17159__17160)
  }
};
cljs.core.map_QMARK_ = function map_QMARK_(x) {
  if(x == null) {
    return false
  }else {
    var G__17165__17166 = x;
    if(G__17165__17166) {
      if(function() {
        var or__3824__auto____17167 = G__17165__17166.cljs$lang$protocol_mask$partition0$ & 1024;
        if(or__3824__auto____17167) {
          return or__3824__auto____17167
        }else {
          return G__17165__17166.cljs$core$IMap$
        }
      }()) {
        return true
      }else {
        if(!G__17165__17166.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IMap, G__17165__17166)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IMap, G__17165__17166)
    }
  }
};
cljs.core.vector_QMARK_ = function vector_QMARK_(x) {
  var G__17171__17172 = x;
  if(G__17171__17172) {
    if(function() {
      var or__3824__auto____17173 = G__17171__17172.cljs$lang$protocol_mask$partition0$ & 16384;
      if(or__3824__auto____17173) {
        return or__3824__auto____17173
      }else {
        return G__17171__17172.cljs$core$IVector$
      }
    }()) {
      return true
    }else {
      if(!G__17171__17172.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IVector, G__17171__17172)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IVector, G__17171__17172)
  }
};
cljs.core.chunked_seq_QMARK_ = function chunked_seq_QMARK_(x) {
  var G__17177__17178 = x;
  if(G__17177__17178) {
    if(function() {
      var or__3824__auto____17179 = G__17177__17178.cljs$lang$protocol_mask$partition1$ & 512;
      if(or__3824__auto____17179) {
        return or__3824__auto____17179
      }else {
        return G__17177__17178.cljs$core$IChunkedSeq$
      }
    }()) {
      return true
    }else {
      if(!G__17177__17178.cljs$lang$protocol_mask$partition1$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedSeq, G__17177__17178)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedSeq, G__17177__17178)
  }
};
cljs.core.js_obj = function() {
  var js_obj = null;
  var js_obj__0 = function() {
    return{}
  };
  var js_obj__1 = function() {
    var G__17180__delegate = function(keyvals) {
      return cljs.core.apply.call(null, goog.object.create, keyvals)
    };
    var G__17180 = function(var_args) {
      var keyvals = null;
      if(goog.isDef(var_args)) {
        keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__17180__delegate.call(this, keyvals)
    };
    G__17180.cljs$lang$maxFixedArity = 0;
    G__17180.cljs$lang$applyTo = function(arglist__17181) {
      var keyvals = cljs.core.seq(arglist__17181);
      return G__17180__delegate(keyvals)
    };
    G__17180.cljs$lang$arity$variadic = G__17180__delegate;
    return G__17180
  }();
  js_obj = function(var_args) {
    var keyvals = var_args;
    switch(arguments.length) {
      case 0:
        return js_obj__0.call(this);
      default:
        return js_obj__1.cljs$lang$arity$variadic(cljs.core.array_seq(arguments, 0))
    }
    throw"Invalid arity: " + arguments.length;
  };
  js_obj.cljs$lang$maxFixedArity = 0;
  js_obj.cljs$lang$applyTo = js_obj__1.cljs$lang$applyTo;
  js_obj.cljs$lang$arity$0 = js_obj__0;
  js_obj.cljs$lang$arity$variadic = js_obj__1.cljs$lang$arity$variadic;
  return js_obj
}();
cljs.core.js_keys = function js_keys(obj) {
  var keys__17183 = [];
  goog.object.forEach(obj, function(val, key, obj) {
    return keys__17183.push(key)
  });
  return keys__17183
};
cljs.core.js_delete = function js_delete(obj, key) {
  return delete obj[key]
};
cljs.core.array_copy = function array_copy(from, i, to, j, len) {
  var i__17187 = i;
  var j__17188 = j;
  var len__17189 = len;
  while(true) {
    if(len__17189 === 0) {
      return to
    }else {
      to[j__17188] = from[i__17187];
      var G__17190 = i__17187 + 1;
      var G__17191 = j__17188 + 1;
      var G__17192 = len__17189 - 1;
      i__17187 = G__17190;
      j__17188 = G__17191;
      len__17189 = G__17192;
      continue
    }
    break
  }
};
cljs.core.array_copy_downward = function array_copy_downward(from, i, to, j, len) {
  var i__17196 = i + (len - 1);
  var j__17197 = j + (len - 1);
  var len__17198 = len;
  while(true) {
    if(len__17198 === 0) {
      return to
    }else {
      to[j__17197] = from[i__17196];
      var G__17199 = i__17196 - 1;
      var G__17200 = j__17197 - 1;
      var G__17201 = len__17198 - 1;
      i__17196 = G__17199;
      j__17197 = G__17200;
      len__17198 = G__17201;
      continue
    }
    break
  }
};
cljs.core.lookup_sentinel = {};
cljs.core.false_QMARK_ = function false_QMARK_(x) {
  return x === false
};
cljs.core.true_QMARK_ = function true_QMARK_(x) {
  return x === true
};
cljs.core.undefined_QMARK_ = function undefined_QMARK_(x) {
  return void 0 === x
};
cljs.core.seq_QMARK_ = function seq_QMARK_(s) {
  if(s == null) {
    return false
  }else {
    var G__17205__17206 = s;
    if(G__17205__17206) {
      if(function() {
        var or__3824__auto____17207 = G__17205__17206.cljs$lang$protocol_mask$partition0$ & 64;
        if(or__3824__auto____17207) {
          return or__3824__auto____17207
        }else {
          return G__17205__17206.cljs$core$ISeq$
        }
      }()) {
        return true
      }else {
        if(!G__17205__17206.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17205__17206)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17205__17206)
    }
  }
};
cljs.core.seqable_QMARK_ = function seqable_QMARK_(s) {
  var G__17211__17212 = s;
  if(G__17211__17212) {
    if(function() {
      var or__3824__auto____17213 = G__17211__17212.cljs$lang$protocol_mask$partition0$ & 8388608;
      if(or__3824__auto____17213) {
        return or__3824__auto____17213
      }else {
        return G__17211__17212.cljs$core$ISeqable$
      }
    }()) {
      return true
    }else {
      if(!G__17211__17212.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeqable, G__17211__17212)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.ISeqable, G__17211__17212)
  }
};
cljs.core.boolean$ = function boolean$(x) {
  if(cljs.core.truth_(x)) {
    return true
  }else {
    return false
  }
};
cljs.core.string_QMARK_ = function string_QMARK_(x) {
  var and__3822__auto____17216 = goog.isString(x);
  if(and__3822__auto____17216) {
    return!function() {
      var or__3824__auto____17217 = x.charAt(0) === "\ufdd0";
      if(or__3824__auto____17217) {
        return or__3824__auto____17217
      }else {
        return x.charAt(0) === "\ufdd1"
      }
    }()
  }else {
    return and__3822__auto____17216
  }
};
cljs.core.keyword_QMARK_ = function keyword_QMARK_(x) {
  var and__3822__auto____17219 = goog.isString(x);
  if(and__3822__auto____17219) {
    return x.charAt(0) === "\ufdd0"
  }else {
    return and__3822__auto____17219
  }
};
cljs.core.symbol_QMARK_ = function symbol_QMARK_(x) {
  var and__3822__auto____17221 = goog.isString(x);
  if(and__3822__auto____17221) {
    return x.charAt(0) === "\ufdd1"
  }else {
    return and__3822__auto____17221
  }
};
cljs.core.number_QMARK_ = function number_QMARK_(n) {
  return goog.isNumber(n)
};
cljs.core.fn_QMARK_ = function fn_QMARK_(f) {
  return goog.isFunction(f)
};
cljs.core.ifn_QMARK_ = function ifn_QMARK_(f) {
  var or__3824__auto____17226 = cljs.core.fn_QMARK_.call(null, f);
  if(or__3824__auto____17226) {
    return or__3824__auto____17226
  }else {
    var G__17227__17228 = f;
    if(G__17227__17228) {
      if(function() {
        var or__3824__auto____17229 = G__17227__17228.cljs$lang$protocol_mask$partition0$ & 1;
        if(or__3824__auto____17229) {
          return or__3824__auto____17229
        }else {
          return G__17227__17228.cljs$core$IFn$
        }
      }()) {
        return true
      }else {
        if(!G__17227__17228.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IFn, G__17227__17228)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IFn, G__17227__17228)
    }
  }
};
cljs.core.integer_QMARK_ = function integer_QMARK_(n) {
  var and__3822__auto____17233 = cljs.core.number_QMARK_.call(null, n);
  if(and__3822__auto____17233) {
    var and__3822__auto____17234 = !isNaN(n);
    if(and__3822__auto____17234) {
      var and__3822__auto____17235 = !(n === Infinity);
      if(and__3822__auto____17235) {
        return parseFloat(n) === parseInt(n, 10)
      }else {
        return and__3822__auto____17235
      }
    }else {
      return and__3822__auto____17234
    }
  }else {
    return and__3822__auto____17233
  }
};
cljs.core.contains_QMARK_ = function contains_QMARK_(coll, v) {
  if(cljs.core._lookup.call(null, coll, v, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
    return false
  }else {
    return true
  }
};
cljs.core.find = function find(coll, k) {
  if(function() {
    var and__3822__auto____17238 = !(coll == null);
    if(and__3822__auto____17238) {
      var and__3822__auto____17239 = cljs.core.associative_QMARK_.call(null, coll);
      if(and__3822__auto____17239) {
        return cljs.core.contains_QMARK_.call(null, coll, k)
      }else {
        return and__3822__auto____17239
      }
    }else {
      return and__3822__auto____17238
    }
  }()) {
    return cljs.core.PersistentVector.fromArray([k, cljs.core._lookup.call(null, coll, k)], true)
  }else {
    return null
  }
};
cljs.core.distinct_QMARK_ = function() {
  var distinct_QMARK_ = null;
  var distinct_QMARK___1 = function(x) {
    return true
  };
  var distinct_QMARK___2 = function(x, y) {
    return!cljs.core._EQ_.call(null, x, y)
  };
  var distinct_QMARK___3 = function() {
    var G__17248__delegate = function(x, y, more) {
      if(!cljs.core._EQ_.call(null, x, y)) {
        var s__17244 = cljs.core.PersistentHashSet.fromArray([y, x]);
        var xs__17245 = more;
        while(true) {
          var x__17246 = cljs.core.first.call(null, xs__17245);
          var etc__17247 = cljs.core.next.call(null, xs__17245);
          if(cljs.core.truth_(xs__17245)) {
            if(cljs.core.contains_QMARK_.call(null, s__17244, x__17246)) {
              return false
            }else {
              var G__17249 = cljs.core.conj.call(null, s__17244, x__17246);
              var G__17250 = etc__17247;
              s__17244 = G__17249;
              xs__17245 = G__17250;
              continue
            }
          }else {
            return true
          }
          break
        }
      }else {
        return false
      }
    };
    var G__17248 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17248__delegate.call(this, x, y, more)
    };
    G__17248.cljs$lang$maxFixedArity = 2;
    G__17248.cljs$lang$applyTo = function(arglist__17251) {
      var x = cljs.core.first(arglist__17251);
      var y = cljs.core.first(cljs.core.next(arglist__17251));
      var more = cljs.core.rest(cljs.core.next(arglist__17251));
      return G__17248__delegate(x, y, more)
    };
    G__17248.cljs$lang$arity$variadic = G__17248__delegate;
    return G__17248
  }();
  distinct_QMARK_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return distinct_QMARK___1.call(this, x);
      case 2:
        return distinct_QMARK___2.call(this, x, y);
      default:
        return distinct_QMARK___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  distinct_QMARK_.cljs$lang$maxFixedArity = 2;
  distinct_QMARK_.cljs$lang$applyTo = distinct_QMARK___3.cljs$lang$applyTo;
  distinct_QMARK_.cljs$lang$arity$1 = distinct_QMARK___1;
  distinct_QMARK_.cljs$lang$arity$2 = distinct_QMARK___2;
  distinct_QMARK_.cljs$lang$arity$variadic = distinct_QMARK___3.cljs$lang$arity$variadic;
  return distinct_QMARK_
}();
cljs.core.compare = function compare(x, y) {
  if(x === y) {
    return 0
  }else {
    if(x == null) {
      return-1
    }else {
      if(y == null) {
        return 1
      }else {
        if(cljs.core.type.call(null, x) === cljs.core.type.call(null, y)) {
          if(function() {
            var G__17255__17256 = x;
            if(G__17255__17256) {
              if(function() {
                var or__3824__auto____17257 = G__17255__17256.cljs$lang$protocol_mask$partition1$ & 2048;
                if(or__3824__auto____17257) {
                  return or__3824__auto____17257
                }else {
                  return G__17255__17256.cljs$core$IComparable$
                }
              }()) {
                return true
              }else {
                if(!G__17255__17256.cljs$lang$protocol_mask$partition1$) {
                  return cljs.core.type_satisfies_.call(null, cljs.core.IComparable, G__17255__17256)
                }else {
                  return false
                }
              }
            }else {
              return cljs.core.type_satisfies_.call(null, cljs.core.IComparable, G__17255__17256)
            }
          }()) {
            return cljs.core._compare.call(null, x, y)
          }else {
            return goog.array.defaultCompare(x, y)
          }
        }else {
          if("\ufdd0'else") {
            throw new Error("compare on non-nil objects of different types");
          }else {
            return null
          }
        }
      }
    }
  }
};
cljs.core.compare_indexed = function() {
  var compare_indexed = null;
  var compare_indexed__2 = function(xs, ys) {
    var xl__17262 = cljs.core.count.call(null, xs);
    var yl__17263 = cljs.core.count.call(null, ys);
    if(xl__17262 < yl__17263) {
      return-1
    }else {
      if(xl__17262 > yl__17263) {
        return 1
      }else {
        if("\ufdd0'else") {
          return compare_indexed.call(null, xs, ys, xl__17262, 0)
        }else {
          return null
        }
      }
    }
  };
  var compare_indexed__4 = function(xs, ys, len, n) {
    while(true) {
      var d__17264 = cljs.core.compare.call(null, cljs.core.nth.call(null, xs, n), cljs.core.nth.call(null, ys, n));
      if(function() {
        var and__3822__auto____17265 = d__17264 === 0;
        if(and__3822__auto____17265) {
          return n + 1 < len
        }else {
          return and__3822__auto____17265
        }
      }()) {
        var G__17266 = xs;
        var G__17267 = ys;
        var G__17268 = len;
        var G__17269 = n + 1;
        xs = G__17266;
        ys = G__17267;
        len = G__17268;
        n = G__17269;
        continue
      }else {
        return d__17264
      }
      break
    }
  };
  compare_indexed = function(xs, ys, len, n) {
    switch(arguments.length) {
      case 2:
        return compare_indexed__2.call(this, xs, ys);
      case 4:
        return compare_indexed__4.call(this, xs, ys, len, n)
    }
    throw"Invalid arity: " + arguments.length;
  };
  compare_indexed.cljs$lang$arity$2 = compare_indexed__2;
  compare_indexed.cljs$lang$arity$4 = compare_indexed__4;
  return compare_indexed
}();
cljs.core.fn__GT_comparator = function fn__GT_comparator(f) {
  if(cljs.core._EQ_.call(null, f, cljs.core.compare)) {
    return cljs.core.compare
  }else {
    return function(x, y) {
      var r__17271 = f.call(null, x, y);
      if(cljs.core.number_QMARK_.call(null, r__17271)) {
        return r__17271
      }else {
        if(cljs.core.truth_(r__17271)) {
          return-1
        }else {
          if(cljs.core.truth_(f.call(null, y, x))) {
            return 1
          }else {
            return 0
          }
        }
      }
    }
  }
};
cljs.core.sort = function() {
  var sort = null;
  var sort__1 = function(coll) {
    return sort.call(null, cljs.core.compare, coll)
  };
  var sort__2 = function(comp, coll) {
    if(cljs.core.seq.call(null, coll)) {
      var a__17273 = cljs.core.to_array.call(null, coll);
      goog.array.stableSort(a__17273, cljs.core.fn__GT_comparator.call(null, comp));
      return cljs.core.seq.call(null, a__17273)
    }else {
      return cljs.core.List.EMPTY
    }
  };
  sort = function(comp, coll) {
    switch(arguments.length) {
      case 1:
        return sort__1.call(this, comp);
      case 2:
        return sort__2.call(this, comp, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  sort.cljs$lang$arity$1 = sort__1;
  sort.cljs$lang$arity$2 = sort__2;
  return sort
}();
cljs.core.sort_by = function() {
  var sort_by = null;
  var sort_by__2 = function(keyfn, coll) {
    return sort_by.call(null, keyfn, cljs.core.compare, coll)
  };
  var sort_by__3 = function(keyfn, comp, coll) {
    return cljs.core.sort.call(null, function(x, y) {
      return cljs.core.fn__GT_comparator.call(null, comp).call(null, keyfn.call(null, x), keyfn.call(null, y))
    }, coll)
  };
  sort_by = function(keyfn, comp, coll) {
    switch(arguments.length) {
      case 2:
        return sort_by__2.call(this, keyfn, comp);
      case 3:
        return sort_by__3.call(this, keyfn, comp, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  sort_by.cljs$lang$arity$2 = sort_by__2;
  sort_by.cljs$lang$arity$3 = sort_by__3;
  return sort_by
}();
cljs.core.seq_reduce = function() {
  var seq_reduce = null;
  var seq_reduce__2 = function(f, coll) {
    var temp__3971__auto____17279 = cljs.core.seq.call(null, coll);
    if(temp__3971__auto____17279) {
      var s__17280 = temp__3971__auto____17279;
      return cljs.core.reduce.call(null, f, cljs.core.first.call(null, s__17280), cljs.core.next.call(null, s__17280))
    }else {
      return f.call(null)
    }
  };
  var seq_reduce__3 = function(f, val, coll) {
    var val__17281 = val;
    var coll__17282 = cljs.core.seq.call(null, coll);
    while(true) {
      if(coll__17282) {
        var nval__17283 = f.call(null, val__17281, cljs.core.first.call(null, coll__17282));
        if(cljs.core.reduced_QMARK_.call(null, nval__17283)) {
          return cljs.core.deref.call(null, nval__17283)
        }else {
          var G__17284 = nval__17283;
          var G__17285 = cljs.core.next.call(null, coll__17282);
          val__17281 = G__17284;
          coll__17282 = G__17285;
          continue
        }
      }else {
        return val__17281
      }
      break
    }
  };
  seq_reduce = function(f, val, coll) {
    switch(arguments.length) {
      case 2:
        return seq_reduce__2.call(this, f, val);
      case 3:
        return seq_reduce__3.call(this, f, val, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  seq_reduce.cljs$lang$arity$2 = seq_reduce__2;
  seq_reduce.cljs$lang$arity$3 = seq_reduce__3;
  return seq_reduce
}();
cljs.core.shuffle = function shuffle(coll) {
  var a__17287 = cljs.core.to_array.call(null, coll);
  goog.array.shuffle(a__17287);
  return cljs.core.vec.call(null, a__17287)
};
cljs.core.reduce = function() {
  var reduce = null;
  var reduce__2 = function(f, coll) {
    if(function() {
      var G__17294__17295 = coll;
      if(G__17294__17295) {
        if(function() {
          var or__3824__auto____17296 = G__17294__17295.cljs$lang$protocol_mask$partition0$ & 524288;
          if(or__3824__auto____17296) {
            return or__3824__auto____17296
          }else {
            return G__17294__17295.cljs$core$IReduce$
          }
        }()) {
          return true
        }else {
          if(!G__17294__17295.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17294__17295)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17294__17295)
      }
    }()) {
      return cljs.core._reduce.call(null, coll, f)
    }else {
      return cljs.core.seq_reduce.call(null, f, coll)
    }
  };
  var reduce__3 = function(f, val, coll) {
    if(function() {
      var G__17297__17298 = coll;
      if(G__17297__17298) {
        if(function() {
          var or__3824__auto____17299 = G__17297__17298.cljs$lang$protocol_mask$partition0$ & 524288;
          if(or__3824__auto____17299) {
            return or__3824__auto____17299
          }else {
            return G__17297__17298.cljs$core$IReduce$
          }
        }()) {
          return true
        }else {
          if(!G__17297__17298.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17297__17298)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17297__17298)
      }
    }()) {
      return cljs.core._reduce.call(null, coll, f, val)
    }else {
      return cljs.core.seq_reduce.call(null, f, val, coll)
    }
  };
  reduce = function(f, val, coll) {
    switch(arguments.length) {
      case 2:
        return reduce__2.call(this, f, val);
      case 3:
        return reduce__3.call(this, f, val, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  reduce.cljs$lang$arity$2 = reduce__2;
  reduce.cljs$lang$arity$3 = reduce__3;
  return reduce
}();
cljs.core.reduce_kv = function reduce_kv(f, init, coll) {
  return cljs.core._kv_reduce.call(null, coll, f, init)
};
cljs.core._PLUS_ = function() {
  var _PLUS_ = null;
  var _PLUS___0 = function() {
    return 0
  };
  var _PLUS___1 = function(x) {
    return x
  };
  var _PLUS___2 = function(x, y) {
    return x + y
  };
  var _PLUS___3 = function() {
    var G__17300__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _PLUS_, x + y, more)
    };
    var G__17300 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17300__delegate.call(this, x, y, more)
    };
    G__17300.cljs$lang$maxFixedArity = 2;
    G__17300.cljs$lang$applyTo = function(arglist__17301) {
      var x = cljs.core.first(arglist__17301);
      var y = cljs.core.first(cljs.core.next(arglist__17301));
      var more = cljs.core.rest(cljs.core.next(arglist__17301));
      return G__17300__delegate(x, y, more)
    };
    G__17300.cljs$lang$arity$variadic = G__17300__delegate;
    return G__17300
  }();
  _PLUS_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 0:
        return _PLUS___0.call(this);
      case 1:
        return _PLUS___1.call(this, x);
      case 2:
        return _PLUS___2.call(this, x, y);
      default:
        return _PLUS___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _PLUS_.cljs$lang$maxFixedArity = 2;
  _PLUS_.cljs$lang$applyTo = _PLUS___3.cljs$lang$applyTo;
  _PLUS_.cljs$lang$arity$0 = _PLUS___0;
  _PLUS_.cljs$lang$arity$1 = _PLUS___1;
  _PLUS_.cljs$lang$arity$2 = _PLUS___2;
  _PLUS_.cljs$lang$arity$variadic = _PLUS___3.cljs$lang$arity$variadic;
  return _PLUS_
}();
cljs.core._ = function() {
  var _ = null;
  var ___1 = function(x) {
    return-x
  };
  var ___2 = function(x, y) {
    return x - y
  };
  var ___3 = function() {
    var G__17302__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _, x - y, more)
    };
    var G__17302 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17302__delegate.call(this, x, y, more)
    };
    G__17302.cljs$lang$maxFixedArity = 2;
    G__17302.cljs$lang$applyTo = function(arglist__17303) {
      var x = cljs.core.first(arglist__17303);
      var y = cljs.core.first(cljs.core.next(arglist__17303));
      var more = cljs.core.rest(cljs.core.next(arglist__17303));
      return G__17302__delegate(x, y, more)
    };
    G__17302.cljs$lang$arity$variadic = G__17302__delegate;
    return G__17302
  }();
  _ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return ___1.call(this, x);
      case 2:
        return ___2.call(this, x, y);
      default:
        return ___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _.cljs$lang$maxFixedArity = 2;
  _.cljs$lang$applyTo = ___3.cljs$lang$applyTo;
  _.cljs$lang$arity$1 = ___1;
  _.cljs$lang$arity$2 = ___2;
  _.cljs$lang$arity$variadic = ___3.cljs$lang$arity$variadic;
  return _
}();
cljs.core._STAR_ = function() {
  var _STAR_ = null;
  var _STAR___0 = function() {
    return 1
  };
  var _STAR___1 = function(x) {
    return x
  };
  var _STAR___2 = function(x, y) {
    return x * y
  };
  var _STAR___3 = function() {
    var G__17304__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _STAR_, x * y, more)
    };
    var G__17304 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17304__delegate.call(this, x, y, more)
    };
    G__17304.cljs$lang$maxFixedArity = 2;
    G__17304.cljs$lang$applyTo = function(arglist__17305) {
      var x = cljs.core.first(arglist__17305);
      var y = cljs.core.first(cljs.core.next(arglist__17305));
      var more = cljs.core.rest(cljs.core.next(arglist__17305));
      return G__17304__delegate(x, y, more)
    };
    G__17304.cljs$lang$arity$variadic = G__17304__delegate;
    return G__17304
  }();
  _STAR_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 0:
        return _STAR___0.call(this);
      case 1:
        return _STAR___1.call(this, x);
      case 2:
        return _STAR___2.call(this, x, y);
      default:
        return _STAR___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _STAR_.cljs$lang$maxFixedArity = 2;
  _STAR_.cljs$lang$applyTo = _STAR___3.cljs$lang$applyTo;
  _STAR_.cljs$lang$arity$0 = _STAR___0;
  _STAR_.cljs$lang$arity$1 = _STAR___1;
  _STAR_.cljs$lang$arity$2 = _STAR___2;
  _STAR_.cljs$lang$arity$variadic = _STAR___3.cljs$lang$arity$variadic;
  return _STAR_
}();
cljs.core._SLASH_ = function() {
  var _SLASH_ = null;
  var _SLASH___1 = function(x) {
    return _SLASH_.call(null, 1, x)
  };
  var _SLASH___2 = function(x, y) {
    return x / y
  };
  var _SLASH___3 = function() {
    var G__17306__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _SLASH_, _SLASH_.call(null, x, y), more)
    };
    var G__17306 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17306__delegate.call(this, x, y, more)
    };
    G__17306.cljs$lang$maxFixedArity = 2;
    G__17306.cljs$lang$applyTo = function(arglist__17307) {
      var x = cljs.core.first(arglist__17307);
      var y = cljs.core.first(cljs.core.next(arglist__17307));
      var more = cljs.core.rest(cljs.core.next(arglist__17307));
      return G__17306__delegate(x, y, more)
    };
    G__17306.cljs$lang$arity$variadic = G__17306__delegate;
    return G__17306
  }();
  _SLASH_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _SLASH___1.call(this, x);
      case 2:
        return _SLASH___2.call(this, x, y);
      default:
        return _SLASH___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _SLASH_.cljs$lang$maxFixedArity = 2;
  _SLASH_.cljs$lang$applyTo = _SLASH___3.cljs$lang$applyTo;
  _SLASH_.cljs$lang$arity$1 = _SLASH___1;
  _SLASH_.cljs$lang$arity$2 = _SLASH___2;
  _SLASH_.cljs$lang$arity$variadic = _SLASH___3.cljs$lang$arity$variadic;
  return _SLASH_
}();
cljs.core._LT_ = function() {
  var _LT_ = null;
  var _LT___1 = function(x) {
    return true
  };
  var _LT___2 = function(x, y) {
    return x < y
  };
  var _LT___3 = function() {
    var G__17308__delegate = function(x, y, more) {
      while(true) {
        if(x < y) {
          if(cljs.core.next.call(null, more)) {
            var G__17309 = y;
            var G__17310 = cljs.core.first.call(null, more);
            var G__17311 = cljs.core.next.call(null, more);
            x = G__17309;
            y = G__17310;
            more = G__17311;
            continue
          }else {
            return y < cljs.core.first.call(null, more)
          }
        }else {
          return false
        }
        break
      }
    };
    var G__17308 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17308__delegate.call(this, x, y, more)
    };
    G__17308.cljs$lang$maxFixedArity = 2;
    G__17308.cljs$lang$applyTo = function(arglist__17312) {
      var x = cljs.core.first(arglist__17312);
      var y = cljs.core.first(cljs.core.next(arglist__17312));
      var more = cljs.core.rest(cljs.core.next(arglist__17312));
      return G__17308__delegate(x, y, more)
    };
    G__17308.cljs$lang$arity$variadic = G__17308__delegate;
    return G__17308
  }();
  _LT_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _LT___1.call(this, x);
      case 2:
        return _LT___2.call(this, x, y);
      default:
        return _LT___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _LT_.cljs$lang$maxFixedArity = 2;
  _LT_.cljs$lang$applyTo = _LT___3.cljs$lang$applyTo;
  _LT_.cljs$lang$arity$1 = _LT___1;
  _LT_.cljs$lang$arity$2 = _LT___2;
  _LT_.cljs$lang$arity$variadic = _LT___3.cljs$lang$arity$variadic;
  return _LT_
}();
cljs.core._LT__EQ_ = function() {
  var _LT__EQ_ = null;
  var _LT__EQ___1 = function(x) {
    return true
  };
  var _LT__EQ___2 = function(x, y) {
    return x <= y
  };
  var _LT__EQ___3 = function() {
    var G__17313__delegate = function(x, y, more) {
      while(true) {
        if(x <= y) {
          if(cljs.core.next.call(null, more)) {
            var G__17314 = y;
            var G__17315 = cljs.core.first.call(null, more);
            var G__17316 = cljs.core.next.call(null, more);
            x = G__17314;
            y = G__17315;
            more = G__17316;
            continue
          }else {
            return y <= cljs.core.first.call(null, more)
          }
        }else {
          return false
        }
        break
      }
    };
    var G__17313 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17313__delegate.call(this, x, y, more)
    };
    G__17313.cljs$lang$maxFixedArity = 2;
    G__17313.cljs$lang$applyTo = function(arglist__17317) {
      var x = cljs.core.first(arglist__17317);
      var y = cljs.core.first(cljs.core.next(arglist__17317));
      var more = cljs.core.rest(cljs.core.next(arglist__17317));
      return G__17313__delegate(x, y, more)
    };
    G__17313.cljs$lang$arity$variadic = G__17313__delegate;
    return G__17313
  }();
  _LT__EQ_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _LT__EQ___1.call(this, x);
      case 2:
        return _LT__EQ___2.call(this, x, y);
      default:
        return _LT__EQ___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _LT__EQ_.cljs$lang$maxFixedArity = 2;
  _LT__EQ_.cljs$lang$applyTo = _LT__EQ___3.cljs$lang$applyTo;
  _LT__EQ_.cljs$lang$arity$1 = _LT__EQ___1;
  _LT__EQ_.cljs$lang$arity$2 = _LT__EQ___2;
  _LT__EQ_.cljs$lang$arity$variadic = _LT__EQ___3.cljs$lang$arity$variadic;
  return _LT__EQ_
}();
cljs.core._GT_ = function() {
  var _GT_ = null;
  var _GT___1 = function(x) {
    return true
  };
  var _GT___2 = function(x, y) {
    return x > y
  };
  var _GT___3 = function() {
    var G__17318__delegate = function(x, y, more) {
      while(true) {
        if(x > y) {
          if(cljs.core.next.call(null, more)) {
            var G__17319 = y;
            var G__17320 = cljs.core.first.call(null, more);
            var G__17321 = cljs.core.next.call(null, more);
            x = G__17319;
            y = G__17320;
            more = G__17321;
            continue
          }else {
            return y > cljs.core.first.call(null, more)
          }
        }else {
          return false
        }
        break
      }
    };
    var G__17318 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17318__delegate.call(this, x, y, more)
    };
    G__17318.cljs$lang$maxFixedArity = 2;
    G__17318.cljs$lang$applyTo = function(arglist__17322) {
      var x = cljs.core.first(arglist__17322);
      var y = cljs.core.first(cljs.core.next(arglist__17322));
      var more = cljs.core.rest(cljs.core.next(arglist__17322));
      return G__17318__delegate(x, y, more)
    };
    G__17318.cljs$lang$arity$variadic = G__17318__delegate;
    return G__17318
  }();
  _GT_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _GT___1.call(this, x);
      case 2:
        return _GT___2.call(this, x, y);
      default:
        return _GT___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _GT_.cljs$lang$maxFixedArity = 2;
  _GT_.cljs$lang$applyTo = _GT___3.cljs$lang$applyTo;
  _GT_.cljs$lang$arity$1 = _GT___1;
  _GT_.cljs$lang$arity$2 = _GT___2;
  _GT_.cljs$lang$arity$variadic = _GT___3.cljs$lang$arity$variadic;
  return _GT_
}();
cljs.core._GT__EQ_ = function() {
  var _GT__EQ_ = null;
  var _GT__EQ___1 = function(x) {
    return true
  };
  var _GT__EQ___2 = function(x, y) {
    return x >= y
  };
  var _GT__EQ___3 = function() {
    var G__17323__delegate = function(x, y, more) {
      while(true) {
        if(x >= y) {
          if(cljs.core.next.call(null, more)) {
            var G__17324 = y;
            var G__17325 = cljs.core.first.call(null, more);
            var G__17326 = cljs.core.next.call(null, more);
            x = G__17324;
            y = G__17325;
            more = G__17326;
            continue
          }else {
            return y >= cljs.core.first.call(null, more)
          }
        }else {
          return false
        }
        break
      }
    };
    var G__17323 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17323__delegate.call(this, x, y, more)
    };
    G__17323.cljs$lang$maxFixedArity = 2;
    G__17323.cljs$lang$applyTo = function(arglist__17327) {
      var x = cljs.core.first(arglist__17327);
      var y = cljs.core.first(cljs.core.next(arglist__17327));
      var more = cljs.core.rest(cljs.core.next(arglist__17327));
      return G__17323__delegate(x, y, more)
    };
    G__17323.cljs$lang$arity$variadic = G__17323__delegate;
    return G__17323
  }();
  _GT__EQ_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _GT__EQ___1.call(this, x);
      case 2:
        return _GT__EQ___2.call(this, x, y);
      default:
        return _GT__EQ___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _GT__EQ_.cljs$lang$maxFixedArity = 2;
  _GT__EQ_.cljs$lang$applyTo = _GT__EQ___3.cljs$lang$applyTo;
  _GT__EQ_.cljs$lang$arity$1 = _GT__EQ___1;
  _GT__EQ_.cljs$lang$arity$2 = _GT__EQ___2;
  _GT__EQ_.cljs$lang$arity$variadic = _GT__EQ___3.cljs$lang$arity$variadic;
  return _GT__EQ_
}();
cljs.core.dec = function dec(x) {
  return x - 1
};
cljs.core.max = function() {
  var max = null;
  var max__1 = function(x) {
    return x
  };
  var max__2 = function(x, y) {
    return x > y ? x : y
  };
  var max__3 = function() {
    var G__17328__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, max, x > y ? x : y, more)
    };
    var G__17328 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17328__delegate.call(this, x, y, more)
    };
    G__17328.cljs$lang$maxFixedArity = 2;
    G__17328.cljs$lang$applyTo = function(arglist__17329) {
      var x = cljs.core.first(arglist__17329);
      var y = cljs.core.first(cljs.core.next(arglist__17329));
      var more = cljs.core.rest(cljs.core.next(arglist__17329));
      return G__17328__delegate(x, y, more)
    };
    G__17328.cljs$lang$arity$variadic = G__17328__delegate;
    return G__17328
  }();
  max = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return max__1.call(this, x);
      case 2:
        return max__2.call(this, x, y);
      default:
        return max__3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  max.cljs$lang$maxFixedArity = 2;
  max.cljs$lang$applyTo = max__3.cljs$lang$applyTo;
  max.cljs$lang$arity$1 = max__1;
  max.cljs$lang$arity$2 = max__2;
  max.cljs$lang$arity$variadic = max__3.cljs$lang$arity$variadic;
  return max
}();
cljs.core.min = function() {
  var min = null;
  var min__1 = function(x) {
    return x
  };
  var min__2 = function(x, y) {
    return x < y ? x : y
  };
  var min__3 = function() {
    var G__17330__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, min, x < y ? x : y, more)
    };
    var G__17330 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17330__delegate.call(this, x, y, more)
    };
    G__17330.cljs$lang$maxFixedArity = 2;
    G__17330.cljs$lang$applyTo = function(arglist__17331) {
      var x = cljs.core.first(arglist__17331);
      var y = cljs.core.first(cljs.core.next(arglist__17331));
      var more = cljs.core.rest(cljs.core.next(arglist__17331));
      return G__17330__delegate(x, y, more)
    };
    G__17330.cljs$lang$arity$variadic = G__17330__delegate;
    return G__17330
  }();
  min = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return min__1.call(this, x);
      case 2:
        return min__2.call(this, x, y);
      default:
        return min__3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  min.cljs$lang$maxFixedArity = 2;
  min.cljs$lang$applyTo = min__3.cljs$lang$applyTo;
  min.cljs$lang$arity$1 = min__1;
  min.cljs$lang$arity$2 = min__2;
  min.cljs$lang$arity$variadic = min__3.cljs$lang$arity$variadic;
  return min
}();
cljs.core.fix = function fix(q) {
  if(q >= 0) {
    return Math.floor.call(null, q)
  }else {
    return Math.ceil.call(null, q)
  }
};
cljs.core.int$ = function int$(x) {
  return cljs.core.fix.call(null, x)
};
cljs.core.long$ = function long$(x) {
  return cljs.core.fix.call(null, x)
};
cljs.core.mod = function mod(n, d) {
  return n % d
};
cljs.core.quot = function quot(n, d) {
  var rem__17333 = n % d;
  return cljs.core.fix.call(null, (n - rem__17333) / d)
};
cljs.core.rem = function rem(n, d) {
  var q__17335 = cljs.core.quot.call(null, n, d);
  return n - d * q__17335
};
cljs.core.rand = function() {
  var rand = null;
  var rand__0 = function() {
    return Math.random.call(null)
  };
  var rand__1 = function(n) {
    return n * rand.call(null)
  };
  rand = function(n) {
    switch(arguments.length) {
      case 0:
        return rand__0.call(this);
      case 1:
        return rand__1.call(this, n)
    }
    throw"Invalid arity: " + arguments.length;
  };
  rand.cljs$lang$arity$0 = rand__0;
  rand.cljs$lang$arity$1 = rand__1;
  return rand
}();
cljs.core.rand_int = function rand_int(n) {
  return cljs.core.fix.call(null, cljs.core.rand.call(null, n))
};
cljs.core.bit_xor = function bit_xor(x, y) {
  return x ^ y
};
cljs.core.bit_and = function bit_and(x, y) {
  return x & y
};
cljs.core.bit_or = function bit_or(x, y) {
  return x | y
};
cljs.core.bit_and_not = function bit_and_not(x, y) {
  return x & ~y
};
cljs.core.bit_clear = function bit_clear(x, n) {
  return x & ~(1 << n)
};
cljs.core.bit_flip = function bit_flip(x, n) {
  return x ^ 1 << n
};
cljs.core.bit_not = function bit_not(x) {
  return~x
};
cljs.core.bit_set = function bit_set(x, n) {
  return x | 1 << n
};
cljs.core.bit_test = function bit_test(x, n) {
  return(x & 1 << n) != 0
};
cljs.core.bit_shift_left = function bit_shift_left(x, n) {
  return x << n
};
cljs.core.bit_shift_right = function bit_shift_right(x, n) {
  return x >> n
};
cljs.core.bit_shift_right_zero_fill = function bit_shift_right_zero_fill(x, n) {
  return x >>> n
};
cljs.core.bit_count = function bit_count(v) {
  var v__17338 = v - (v >> 1 & 1431655765);
  var v__17339 = (v__17338 & 858993459) + (v__17338 >> 2 & 858993459);
  return(v__17339 + (v__17339 >> 4) & 252645135) * 16843009 >> 24
};
cljs.core._EQ__EQ_ = function() {
  var _EQ__EQ_ = null;
  var _EQ__EQ___1 = function(x) {
    return true
  };
  var _EQ__EQ___2 = function(x, y) {
    return cljs.core._equiv.call(null, x, y)
  };
  var _EQ__EQ___3 = function() {
    var G__17340__delegate = function(x, y, more) {
      while(true) {
        if(cljs.core.truth_(_EQ__EQ_.call(null, x, y))) {
          if(cljs.core.next.call(null, more)) {
            var G__17341 = y;
            var G__17342 = cljs.core.first.call(null, more);
            var G__17343 = cljs.core.next.call(null, more);
            x = G__17341;
            y = G__17342;
            more = G__17343;
            continue
          }else {
            return _EQ__EQ_.call(null, y, cljs.core.first.call(null, more))
          }
        }else {
          return false
        }
        break
      }
    };
    var G__17340 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17340__delegate.call(this, x, y, more)
    };
    G__17340.cljs$lang$maxFixedArity = 2;
    G__17340.cljs$lang$applyTo = function(arglist__17344) {
      var x = cljs.core.first(arglist__17344);
      var y = cljs.core.first(cljs.core.next(arglist__17344));
      var more = cljs.core.rest(cljs.core.next(arglist__17344));
      return G__17340__delegate(x, y, more)
    };
    G__17340.cljs$lang$arity$variadic = G__17340__delegate;
    return G__17340
  }();
  _EQ__EQ_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return _EQ__EQ___1.call(this, x);
      case 2:
        return _EQ__EQ___2.call(this, x, y);
      default:
        return _EQ__EQ___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  _EQ__EQ_.cljs$lang$maxFixedArity = 2;
  _EQ__EQ_.cljs$lang$applyTo = _EQ__EQ___3.cljs$lang$applyTo;
  _EQ__EQ_.cljs$lang$arity$1 = _EQ__EQ___1;
  _EQ__EQ_.cljs$lang$arity$2 = _EQ__EQ___2;
  _EQ__EQ_.cljs$lang$arity$variadic = _EQ__EQ___3.cljs$lang$arity$variadic;
  return _EQ__EQ_
}();
cljs.core.pos_QMARK_ = function pos_QMARK_(n) {
  return n > 0
};
cljs.core.zero_QMARK_ = function zero_QMARK_(n) {
  return n === 0
};
cljs.core.neg_QMARK_ = function neg_QMARK_(x) {
  return x < 0
};
cljs.core.nthnext = function nthnext(coll, n) {
  var n__17348 = n;
  var xs__17349 = cljs.core.seq.call(null, coll);
  while(true) {
    if(cljs.core.truth_(function() {
      var and__3822__auto____17350 = xs__17349;
      if(and__3822__auto____17350) {
        return n__17348 > 0
      }else {
        return and__3822__auto____17350
      }
    }())) {
      var G__17351 = n__17348 - 1;
      var G__17352 = cljs.core.next.call(null, xs__17349);
      n__17348 = G__17351;
      xs__17349 = G__17352;
      continue
    }else {
      return xs__17349
    }
    break
  }
};
cljs.core.str_STAR_ = function() {
  var str_STAR_ = null;
  var str_STAR___0 = function() {
    return""
  };
  var str_STAR___1 = function(x) {
    if(x == null) {
      return""
    }else {
      if("\ufdd0'else") {
        return x.toString()
      }else {
        return null
      }
    }
  };
  var str_STAR___2 = function() {
    var G__17353__delegate = function(x, ys) {
      return function(sb, more) {
        while(true) {
          if(cljs.core.truth_(more)) {
            var G__17354 = sb.append(str_STAR_.call(null, cljs.core.first.call(null, more)));
            var G__17355 = cljs.core.next.call(null, more);
            sb = G__17354;
            more = G__17355;
            continue
          }else {
            return str_STAR_.call(null, sb)
          }
          break
        }
      }.call(null, new goog.string.StringBuffer(str_STAR_.call(null, x)), ys)
    };
    var G__17353 = function(x, var_args) {
      var ys = null;
      if(goog.isDef(var_args)) {
        ys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__17353__delegate.call(this, x, ys)
    };
    G__17353.cljs$lang$maxFixedArity = 1;
    G__17353.cljs$lang$applyTo = function(arglist__17356) {
      var x = cljs.core.first(arglist__17356);
      var ys = cljs.core.rest(arglist__17356);
      return G__17353__delegate(x, ys)
    };
    G__17353.cljs$lang$arity$variadic = G__17353__delegate;
    return G__17353
  }();
  str_STAR_ = function(x, var_args) {
    var ys = var_args;
    switch(arguments.length) {
      case 0:
        return str_STAR___0.call(this);
      case 1:
        return str_STAR___1.call(this, x);
      default:
        return str_STAR___2.cljs$lang$arity$variadic(x, cljs.core.array_seq(arguments, 1))
    }
    throw"Invalid arity: " + arguments.length;
  };
  str_STAR_.cljs$lang$maxFixedArity = 1;
  str_STAR_.cljs$lang$applyTo = str_STAR___2.cljs$lang$applyTo;
  str_STAR_.cljs$lang$arity$0 = str_STAR___0;
  str_STAR_.cljs$lang$arity$1 = str_STAR___1;
  str_STAR_.cljs$lang$arity$variadic = str_STAR___2.cljs$lang$arity$variadic;
  return str_STAR_
}();
cljs.core.str = function() {
  var str = null;
  var str__0 = function() {
    return""
  };
  var str__1 = function(x) {
    if(cljs.core.symbol_QMARK_.call(null, x)) {
      return x.substring(2, x.length)
    }else {
      if(cljs.core.keyword_QMARK_.call(null, x)) {
        return cljs.core.str_STAR_.call(null, ":", x.substring(2, x.length))
      }else {
        if(x == null) {
          return""
        }else {
          if("\ufdd0'else") {
            return x.toString()
          }else {
            return null
          }
        }
      }
    }
  };
  var str__2 = function() {
    var G__17357__delegate = function(x, ys) {
      return function(sb, more) {
        while(true) {
          if(cljs.core.truth_(more)) {
            var G__17358 = sb.append(str.call(null, cljs.core.first.call(null, more)));
            var G__17359 = cljs.core.next.call(null, more);
            sb = G__17358;
            more = G__17359;
            continue
          }else {
            return cljs.core.str_STAR_.call(null, sb)
          }
          break
        }
      }.call(null, new goog.string.StringBuffer(str.call(null, x)), ys)
    };
    var G__17357 = function(x, var_args) {
      var ys = null;
      if(goog.isDef(var_args)) {
        ys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__17357__delegate.call(this, x, ys)
    };
    G__17357.cljs$lang$maxFixedArity = 1;
    G__17357.cljs$lang$applyTo = function(arglist__17360) {
      var x = cljs.core.first(arglist__17360);
      var ys = cljs.core.rest(arglist__17360);
      return G__17357__delegate(x, ys)
    };
    G__17357.cljs$lang$arity$variadic = G__17357__delegate;
    return G__17357
  }();
  str = function(x, var_args) {
    var ys = var_args;
    switch(arguments.length) {
      case 0:
        return str__0.call(this);
      case 1:
        return str__1.call(this, x);
      default:
        return str__2.cljs$lang$arity$variadic(x, cljs.core.array_seq(arguments, 1))
    }
    throw"Invalid arity: " + arguments.length;
  };
  str.cljs$lang$maxFixedArity = 1;
  str.cljs$lang$applyTo = str__2.cljs$lang$applyTo;
  str.cljs$lang$arity$0 = str__0;
  str.cljs$lang$arity$1 = str__1;
  str.cljs$lang$arity$variadic = str__2.cljs$lang$arity$variadic;
  return str
}();
cljs.core.subs = function() {
  var subs = null;
  var subs__2 = function(s, start) {
    return s.substring(start)
  };
  var subs__3 = function(s, start, end) {
    return s.substring(start, end)
  };
  subs = function(s, start, end) {
    switch(arguments.length) {
      case 2:
        return subs__2.call(this, s, start);
      case 3:
        return subs__3.call(this, s, start, end)
    }
    throw"Invalid arity: " + arguments.length;
  };
  subs.cljs$lang$arity$2 = subs__2;
  subs.cljs$lang$arity$3 = subs__3;
  return subs
}();
cljs.core.format = function() {
  var format__delegate = function(fmt, args) {
    var args__17364 = cljs.core.map.call(null, function(x) {
      if(function() {
        var or__3824__auto____17363 = cljs.core.keyword_QMARK_.call(null, x);
        if(or__3824__auto____17363) {
          return or__3824__auto____17363
        }else {
          return cljs.core.symbol_QMARK_.call(null, x)
        }
      }()) {
        return[cljs.core.str(x)].join("")
      }else {
        return x
      }
    }, args);
    return cljs.core.apply.call(null, goog.string.format, fmt, args__17364)
  };
  var format = function(fmt, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return format__delegate.call(this, fmt, args)
  };
  format.cljs$lang$maxFixedArity = 1;
  format.cljs$lang$applyTo = function(arglist__17365) {
    var fmt = cljs.core.first(arglist__17365);
    var args = cljs.core.rest(arglist__17365);
    return format__delegate(fmt, args)
  };
  format.cljs$lang$arity$variadic = format__delegate;
  return format
}();
cljs.core.symbol = function() {
  var symbol = null;
  var symbol__1 = function(name) {
    if(cljs.core.symbol_QMARK_.call(null, name)) {
      return name
    }else {
      if(cljs.core.keyword_QMARK_.call(null, name)) {
        return cljs.core.str_STAR_.call(null, "\ufdd1", "'", cljs.core.subs.call(null, name, 2))
      }else {
        if("\ufdd0'else") {
          return cljs.core.str_STAR_.call(null, "\ufdd1", "'", name)
        }else {
          return null
        }
      }
    }
  };
  var symbol__2 = function(ns, name) {
    return symbol.call(null, cljs.core.str_STAR_.call(null, ns, "/", name))
  };
  symbol = function(ns, name) {
    switch(arguments.length) {
      case 1:
        return symbol__1.call(this, ns);
      case 2:
        return symbol__2.call(this, ns, name)
    }
    throw"Invalid arity: " + arguments.length;
  };
  symbol.cljs$lang$arity$1 = symbol__1;
  symbol.cljs$lang$arity$2 = symbol__2;
  return symbol
}();
cljs.core.keyword = function() {
  var keyword = null;
  var keyword__1 = function(name) {
    if(cljs.core.keyword_QMARK_.call(null, name)) {
      return name
    }else {
      if(cljs.core.symbol_QMARK_.call(null, name)) {
        return cljs.core.str_STAR_.call(null, "\ufdd0", "'", cljs.core.subs.call(null, name, 2))
      }else {
        if("\ufdd0'else") {
          return cljs.core.str_STAR_.call(null, "\ufdd0", "'", name)
        }else {
          return null
        }
      }
    }
  };
  var keyword__2 = function(ns, name) {
    return keyword.call(null, cljs.core.str_STAR_.call(null, ns, "/", name))
  };
  keyword = function(ns, name) {
    switch(arguments.length) {
      case 1:
        return keyword__1.call(this, ns);
      case 2:
        return keyword__2.call(this, ns, name)
    }
    throw"Invalid arity: " + arguments.length;
  };
  keyword.cljs$lang$arity$1 = keyword__1;
  keyword.cljs$lang$arity$2 = keyword__2;
  return keyword
}();
cljs.core.equiv_sequential = function equiv_sequential(x, y) {
  return cljs.core.boolean$.call(null, cljs.core.sequential_QMARK_.call(null, y) ? function() {
    var xs__17368 = cljs.core.seq.call(null, x);
    var ys__17369 = cljs.core.seq.call(null, y);
    while(true) {
      if(xs__17368 == null) {
        return ys__17369 == null
      }else {
        if(ys__17369 == null) {
          return false
        }else {
          if(cljs.core._EQ_.call(null, cljs.core.first.call(null, xs__17368), cljs.core.first.call(null, ys__17369))) {
            var G__17370 = cljs.core.next.call(null, xs__17368);
            var G__17371 = cljs.core.next.call(null, ys__17369);
            xs__17368 = G__17370;
            ys__17369 = G__17371;
            continue
          }else {
            if("\ufdd0'else") {
              return false
            }else {
              return null
            }
          }
        }
      }
      break
    }
  }() : null)
};
cljs.core.hash_combine = function hash_combine(seed, hash) {
  return seed ^ hash + 2654435769 + (seed << 6) + (seed >> 2)
};
cljs.core.hash_coll = function hash_coll(coll) {
  return cljs.core.reduce.call(null, function(p1__17372_SHARP_, p2__17373_SHARP_) {
    return cljs.core.hash_combine.call(null, p1__17372_SHARP_, cljs.core.hash.call(null, p2__17373_SHARP_, false))
  }, cljs.core.hash.call(null, cljs.core.first.call(null, coll), false), cljs.core.next.call(null, coll))
};
cljs.core.hash_imap = function hash_imap(m) {
  var h__17377 = 0;
  var s__17378 = cljs.core.seq.call(null, m);
  while(true) {
    if(s__17378) {
      var e__17379 = cljs.core.first.call(null, s__17378);
      var G__17380 = (h__17377 + (cljs.core.hash.call(null, cljs.core.key.call(null, e__17379)) ^ cljs.core.hash.call(null, cljs.core.val.call(null, e__17379)))) % 4503599627370496;
      var G__17381 = cljs.core.next.call(null, s__17378);
      h__17377 = G__17380;
      s__17378 = G__17381;
      continue
    }else {
      return h__17377
    }
    break
  }
};
cljs.core.hash_iset = function hash_iset(s) {
  var h__17385 = 0;
  var s__17386 = cljs.core.seq.call(null, s);
  while(true) {
    if(s__17386) {
      var e__17387 = cljs.core.first.call(null, s__17386);
      var G__17388 = (h__17385 + cljs.core.hash.call(null, e__17387)) % 4503599627370496;
      var G__17389 = cljs.core.next.call(null, s__17386);
      h__17385 = G__17388;
      s__17386 = G__17389;
      continue
    }else {
      return h__17385
    }
    break
  }
};
cljs.core.extend_object_BANG_ = function extend_object_BANG_(obj, fn_map) {
  var G__17397__17398 = cljs.core.seq.call(null, fn_map);
  while(true) {
    if(G__17397__17398) {
      var vec__17399__17400 = cljs.core.first.call(null, G__17397__17398);
      var key_name__17401 = cljs.core.nth.call(null, vec__17399__17400, 0, null);
      var f__17402 = cljs.core.nth.call(null, vec__17399__17400, 1, null);
      var str_name__17403 = cljs.core.name.call(null, key_name__17401);
      obj[str_name__17403] = f__17402;
      var G__17404 = cljs.core.next.call(null, G__17397__17398);
      G__17397__17398 = G__17404;
      continue
    }else {
    }
    break
  }
  return obj
};
goog.provide("cljs.core.List");
cljs.core.List = function(meta, first, rest, count, __hash) {
  this.meta = meta;
  this.first = first;
  this.rest = rest;
  this.count = count;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 65413358
};
cljs.core.List.cljs$lang$type = true;
cljs.core.List.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/List")
};
cljs.core.List.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/List")
};
cljs.core.List.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17405 = this;
  var h__2247__auto____17406 = this__17405.__hash;
  if(!(h__2247__auto____17406 == null)) {
    return h__2247__auto____17406
  }else {
    var h__2247__auto____17407 = cljs.core.hash_coll.call(null, coll);
    this__17405.__hash = h__2247__auto____17407;
    return h__2247__auto____17407
  }
};
cljs.core.List.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17408 = this;
  if(this__17408.count === 1) {
    return null
  }else {
    return this__17408.rest
  }
};
cljs.core.List.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17409 = this;
  return new cljs.core.List(this__17409.meta, o, coll, this__17409.count + 1, null)
};
cljs.core.List.prototype.toString = function() {
  var this__17410 = this;
  var this__17411 = this;
  return cljs.core.pr_str.call(null, this__17411)
};
cljs.core.List.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17412 = this;
  return coll
};
cljs.core.List.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__17413 = this;
  return this__17413.count
};
cljs.core.List.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__17414 = this;
  return this__17414.first
};
cljs.core.List.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__17415 = this;
  return coll.cljs$core$ISeq$_rest$arity$1(coll)
};
cljs.core.List.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17416 = this;
  return this__17416.first
};
cljs.core.List.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17417 = this;
  if(this__17417.count === 1) {
    return cljs.core.List.EMPTY
  }else {
    return this__17417.rest
  }
};
cljs.core.List.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17418 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.List.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17419 = this;
  return new cljs.core.List(meta, this__17419.first, this__17419.rest, this__17419.count, this__17419.__hash)
};
cljs.core.List.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17420 = this;
  return this__17420.meta
};
cljs.core.List.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17421 = this;
  return cljs.core.List.EMPTY
};
cljs.core.List;
goog.provide("cljs.core.EmptyList");
cljs.core.EmptyList = function(meta) {
  this.meta = meta;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 65413326
};
cljs.core.EmptyList.cljs$lang$type = true;
cljs.core.EmptyList.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/EmptyList")
};
cljs.core.EmptyList.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/EmptyList")
};
cljs.core.EmptyList.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17422 = this;
  return 0
};
cljs.core.EmptyList.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17423 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17424 = this;
  return new cljs.core.List(this__17424.meta, o, null, 1, null)
};
cljs.core.EmptyList.prototype.toString = function() {
  var this__17425 = this;
  var this__17426 = this;
  return cljs.core.pr_str.call(null, this__17426)
};
cljs.core.EmptyList.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17427 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__17428 = this;
  return 0
};
cljs.core.EmptyList.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__17429 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__17430 = this;
  throw new Error("Can't pop empty list");
};
cljs.core.EmptyList.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17431 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17432 = this;
  return cljs.core.List.EMPTY
};
cljs.core.EmptyList.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17433 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.EmptyList.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17434 = this;
  return new cljs.core.EmptyList(meta)
};
cljs.core.EmptyList.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17435 = this;
  return this__17435.meta
};
cljs.core.EmptyList.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17436 = this;
  return coll
};
cljs.core.EmptyList;
cljs.core.List.EMPTY = new cljs.core.EmptyList(null);
cljs.core.reversible_QMARK_ = function reversible_QMARK_(coll) {
  var G__17440__17441 = coll;
  if(G__17440__17441) {
    if(function() {
      var or__3824__auto____17442 = G__17440__17441.cljs$lang$protocol_mask$partition0$ & 134217728;
      if(or__3824__auto____17442) {
        return or__3824__auto____17442
      }else {
        return G__17440__17441.cljs$core$IReversible$
      }
    }()) {
      return true
    }else {
      if(!G__17440__17441.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReversible, G__17440__17441)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IReversible, G__17440__17441)
  }
};
cljs.core.rseq = function rseq(coll) {
  return cljs.core._rseq.call(null, coll)
};
cljs.core.reverse = function reverse(coll) {
  if(cljs.core.reversible_QMARK_.call(null, coll)) {
    return cljs.core.rseq.call(null, coll)
  }else {
    return cljs.core.reduce.call(null, cljs.core.conj, cljs.core.List.EMPTY, coll)
  }
};
cljs.core.list = function() {
  var list = null;
  var list__0 = function() {
    return cljs.core.List.EMPTY
  };
  var list__1 = function(x) {
    return cljs.core.conj.call(null, cljs.core.List.EMPTY, x)
  };
  var list__2 = function(x, y) {
    return cljs.core.conj.call(null, list.call(null, y), x)
  };
  var list__3 = function(x, y, z) {
    return cljs.core.conj.call(null, list.call(null, y, z), x)
  };
  var list__4 = function() {
    var G__17443__delegate = function(x, y, z, items) {
      return cljs.core.conj.call(null, cljs.core.conj.call(null, cljs.core.conj.call(null, cljs.core.reduce.call(null, cljs.core.conj, cljs.core.List.EMPTY, cljs.core.reverse.call(null, items)), z), y), x)
    };
    var G__17443 = function(x, y, z, var_args) {
      var items = null;
      if(goog.isDef(var_args)) {
        items = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__17443__delegate.call(this, x, y, z, items)
    };
    G__17443.cljs$lang$maxFixedArity = 3;
    G__17443.cljs$lang$applyTo = function(arglist__17444) {
      var x = cljs.core.first(arglist__17444);
      var y = cljs.core.first(cljs.core.next(arglist__17444));
      var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17444)));
      var items = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17444)));
      return G__17443__delegate(x, y, z, items)
    };
    G__17443.cljs$lang$arity$variadic = G__17443__delegate;
    return G__17443
  }();
  list = function(x, y, z, var_args) {
    var items = var_args;
    switch(arguments.length) {
      case 0:
        return list__0.call(this);
      case 1:
        return list__1.call(this, x);
      case 2:
        return list__2.call(this, x, y);
      case 3:
        return list__3.call(this, x, y, z);
      default:
        return list__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  list.cljs$lang$maxFixedArity = 3;
  list.cljs$lang$applyTo = list__4.cljs$lang$applyTo;
  list.cljs$lang$arity$0 = list__0;
  list.cljs$lang$arity$1 = list__1;
  list.cljs$lang$arity$2 = list__2;
  list.cljs$lang$arity$3 = list__3;
  list.cljs$lang$arity$variadic = list__4.cljs$lang$arity$variadic;
  return list
}();
goog.provide("cljs.core.Cons");
cljs.core.Cons = function(meta, first, rest, __hash) {
  this.meta = meta;
  this.first = first;
  this.rest = rest;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 65405164
};
cljs.core.Cons.cljs$lang$type = true;
cljs.core.Cons.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Cons")
};
cljs.core.Cons.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Cons")
};
cljs.core.Cons.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17445 = this;
  var h__2247__auto____17446 = this__17445.__hash;
  if(!(h__2247__auto____17446 == null)) {
    return h__2247__auto____17446
  }else {
    var h__2247__auto____17447 = cljs.core.hash_coll.call(null, coll);
    this__17445.__hash = h__2247__auto____17447;
    return h__2247__auto____17447
  }
};
cljs.core.Cons.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17448 = this;
  if(this__17448.rest == null) {
    return null
  }else {
    return cljs.core._seq.call(null, this__17448.rest)
  }
};
cljs.core.Cons.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17449 = this;
  return new cljs.core.Cons(null, o, coll, this__17449.__hash)
};
cljs.core.Cons.prototype.toString = function() {
  var this__17450 = this;
  var this__17451 = this;
  return cljs.core.pr_str.call(null, this__17451)
};
cljs.core.Cons.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17452 = this;
  return coll
};
cljs.core.Cons.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17453 = this;
  return this__17453.first
};
cljs.core.Cons.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17454 = this;
  if(this__17454.rest == null) {
    return cljs.core.List.EMPTY
  }else {
    return this__17454.rest
  }
};
cljs.core.Cons.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17455 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.Cons.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17456 = this;
  return new cljs.core.Cons(meta, this__17456.first, this__17456.rest, this__17456.__hash)
};
cljs.core.Cons.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17457 = this;
  return this__17457.meta
};
cljs.core.Cons.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17458 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17458.meta)
};
cljs.core.Cons;
cljs.core.cons = function cons(x, coll) {
  if(function() {
    var or__3824__auto____17463 = coll == null;
    if(or__3824__auto____17463) {
      return or__3824__auto____17463
    }else {
      var G__17464__17465 = coll;
      if(G__17464__17465) {
        if(function() {
          var or__3824__auto____17466 = G__17464__17465.cljs$lang$protocol_mask$partition0$ & 64;
          if(or__3824__auto____17466) {
            return or__3824__auto____17466
          }else {
            return G__17464__17465.cljs$core$ISeq$
          }
        }()) {
          return true
        }else {
          if(!G__17464__17465.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17464__17465)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17464__17465)
      }
    }
  }()) {
    return new cljs.core.Cons(null, x, coll, null)
  }else {
    return new cljs.core.Cons(null, x, cljs.core.seq.call(null, coll), null)
  }
};
cljs.core.list_QMARK_ = function list_QMARK_(x) {
  var G__17470__17471 = x;
  if(G__17470__17471) {
    if(function() {
      var or__3824__auto____17472 = G__17470__17471.cljs$lang$protocol_mask$partition0$ & 33554432;
      if(or__3824__auto____17472) {
        return or__3824__auto____17472
      }else {
        return G__17470__17471.cljs$core$IList$
      }
    }()) {
      return true
    }else {
      if(!G__17470__17471.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IList, G__17470__17471)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IList, G__17470__17471)
  }
};
cljs.core.IReduce["string"] = true;
cljs.core._reduce["string"] = function() {
  var G__17473 = null;
  var G__17473__2 = function(string, f) {
    return cljs.core.ci_reduce.call(null, string, f)
  };
  var G__17473__3 = function(string, f, start) {
    return cljs.core.ci_reduce.call(null, string, f, start)
  };
  G__17473 = function(string, f, start) {
    switch(arguments.length) {
      case 2:
        return G__17473__2.call(this, string, f);
      case 3:
        return G__17473__3.call(this, string, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17473
}();
cljs.core.ILookup["string"] = true;
cljs.core._lookup["string"] = function() {
  var G__17474 = null;
  var G__17474__2 = function(string, k) {
    return cljs.core._nth.call(null, string, k)
  };
  var G__17474__3 = function(string, k, not_found) {
    return cljs.core._nth.call(null, string, k, not_found)
  };
  G__17474 = function(string, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17474__2.call(this, string, k);
      case 3:
        return G__17474__3.call(this, string, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17474
}();
cljs.core.IIndexed["string"] = true;
cljs.core._nth["string"] = function() {
  var G__17475 = null;
  var G__17475__2 = function(string, n) {
    if(n < cljs.core._count.call(null, string)) {
      return string.charAt(n)
    }else {
      return null
    }
  };
  var G__17475__3 = function(string, n, not_found) {
    if(n < cljs.core._count.call(null, string)) {
      return string.charAt(n)
    }else {
      return not_found
    }
  };
  G__17475 = function(string, n, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17475__2.call(this, string, n);
      case 3:
        return G__17475__3.call(this, string, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17475
}();
cljs.core.ICounted["string"] = true;
cljs.core._count["string"] = function(s) {
  return s.length
};
cljs.core.ISeqable["string"] = true;
cljs.core._seq["string"] = function(string) {
  return cljs.core.prim_seq.call(null, string, 0)
};
cljs.core.IHash["string"] = true;
cljs.core._hash["string"] = function(o) {
  return goog.string.hashCode(o)
};
goog.provide("cljs.core.Keyword");
cljs.core.Keyword = function(k) {
  this.k = k;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 1
};
cljs.core.Keyword.cljs$lang$type = true;
cljs.core.Keyword.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Keyword")
};
cljs.core.Keyword.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Keyword")
};
cljs.core.Keyword.prototype.call = function() {
  var G__17487 = null;
  var G__17487__2 = function(this_sym17478, coll) {
    var this__17480 = this;
    var this_sym17478__17481 = this;
    var ___17482 = this_sym17478__17481;
    if(coll == null) {
      return null
    }else {
      var strobj__17483 = coll.strobj;
      if(strobj__17483 == null) {
        return cljs.core._lookup.call(null, coll, this__17480.k, null)
      }else {
        return strobj__17483[this__17480.k]
      }
    }
  };
  var G__17487__3 = function(this_sym17479, coll, not_found) {
    var this__17480 = this;
    var this_sym17479__17484 = this;
    var ___17485 = this_sym17479__17484;
    if(coll == null) {
      return not_found
    }else {
      return cljs.core._lookup.call(null, coll, this__17480.k, not_found)
    }
  };
  G__17487 = function(this_sym17479, coll, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17487__2.call(this, this_sym17479, coll);
      case 3:
        return G__17487__3.call(this, this_sym17479, coll, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17487
}();
cljs.core.Keyword.prototype.apply = function(this_sym17476, args17477) {
  var this__17486 = this;
  return this_sym17476.call.apply(this_sym17476, [this_sym17476].concat(args17477.slice()))
};
cljs.core.Keyword;
String.prototype.cljs$core$IFn$ = true;
String.prototype.call = function() {
  var G__17496 = null;
  var G__17496__2 = function(this_sym17490, coll) {
    var this_sym17490__17492 = this;
    var this__17493 = this_sym17490__17492;
    return cljs.core._lookup.call(null, coll, this__17493.toString(), null)
  };
  var G__17496__3 = function(this_sym17491, coll, not_found) {
    var this_sym17491__17494 = this;
    var this__17495 = this_sym17491__17494;
    return cljs.core._lookup.call(null, coll, this__17495.toString(), not_found)
  };
  G__17496 = function(this_sym17491, coll, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17496__2.call(this, this_sym17491, coll);
      case 3:
        return G__17496__3.call(this, this_sym17491, coll, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17496
}();
String.prototype.apply = function(this_sym17488, args17489) {
  return this_sym17488.call.apply(this_sym17488, [this_sym17488].concat(args17489.slice()))
};
String.prototype.apply = function(s, args) {
  if(cljs.core.count.call(null, args) < 2) {
    return cljs.core._lookup.call(null, args[0], s, null)
  }else {
    return cljs.core._lookup.call(null, args[0], s, args[1])
  }
};
cljs.core.lazy_seq_value = function lazy_seq_value(lazy_seq) {
  var x__17498 = lazy_seq.x;
  if(lazy_seq.realized) {
    return x__17498
  }else {
    lazy_seq.x = x__17498.call(null);
    lazy_seq.realized = true;
    return lazy_seq.x
  }
};
goog.provide("cljs.core.LazySeq");
cljs.core.LazySeq = function(meta, realized, x, __hash) {
  this.meta = meta;
  this.realized = realized;
  this.x = x;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850700
};
cljs.core.LazySeq.cljs$lang$type = true;
cljs.core.LazySeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/LazySeq")
};
cljs.core.LazySeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/LazySeq")
};
cljs.core.LazySeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17499 = this;
  var h__2247__auto____17500 = this__17499.__hash;
  if(!(h__2247__auto____17500 == null)) {
    return h__2247__auto____17500
  }else {
    var h__2247__auto____17501 = cljs.core.hash_coll.call(null, coll);
    this__17499.__hash = h__2247__auto____17501;
    return h__2247__auto____17501
  }
};
cljs.core.LazySeq.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17502 = this;
  return cljs.core._seq.call(null, coll.cljs$core$ISeq$_rest$arity$1(coll))
};
cljs.core.LazySeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17503 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.LazySeq.prototype.toString = function() {
  var this__17504 = this;
  var this__17505 = this;
  return cljs.core.pr_str.call(null, this__17505)
};
cljs.core.LazySeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17506 = this;
  return cljs.core.seq.call(null, cljs.core.lazy_seq_value.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17507 = this;
  return cljs.core.first.call(null, cljs.core.lazy_seq_value.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17508 = this;
  return cljs.core.rest.call(null, cljs.core.lazy_seq_value.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17509 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.LazySeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17510 = this;
  return new cljs.core.LazySeq(meta, this__17510.realized, this__17510.x, this__17510.__hash)
};
cljs.core.LazySeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17511 = this;
  return this__17511.meta
};
cljs.core.LazySeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17512 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17512.meta)
};
cljs.core.LazySeq;
goog.provide("cljs.core.ChunkBuffer");
cljs.core.ChunkBuffer = function(buf, end) {
  this.buf = buf;
  this.end = end;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2
};
cljs.core.ChunkBuffer.cljs$lang$type = true;
cljs.core.ChunkBuffer.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ChunkBuffer")
};
cljs.core.ChunkBuffer.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ChunkBuffer")
};
cljs.core.ChunkBuffer.prototype.cljs$core$ICounted$_count$arity$1 = function(_) {
  var this__17513 = this;
  return this__17513.end
};
cljs.core.ChunkBuffer.prototype.add = function(o) {
  var this__17514 = this;
  var ___17515 = this;
  this__17514.buf[this__17514.end] = o;
  return this__17514.end = this__17514.end + 1
};
cljs.core.ChunkBuffer.prototype.chunk = function(o) {
  var this__17516 = this;
  var ___17517 = this;
  var ret__17518 = new cljs.core.ArrayChunk(this__17516.buf, 0, this__17516.end);
  this__17516.buf = null;
  return ret__17518
};
cljs.core.ChunkBuffer;
cljs.core.chunk_buffer = function chunk_buffer(capacity) {
  return new cljs.core.ChunkBuffer(cljs.core.make_array.call(null, capacity), 0)
};
goog.provide("cljs.core.ArrayChunk");
cljs.core.ArrayChunk = function(arr, off, end) {
  this.arr = arr;
  this.off = off;
  this.end = end;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 524306
};
cljs.core.ArrayChunk.cljs$lang$type = true;
cljs.core.ArrayChunk.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ArrayChunk")
};
cljs.core.ArrayChunk.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ArrayChunk")
};
cljs.core.ArrayChunk.prototype.cljs$core$IReduce$_reduce$arity$2 = function(coll, f) {
  var this__17519 = this;
  return cljs.core.array_reduce.call(null, this__17519.arr, f, this__17519.arr[this__17519.off], this__17519.off + 1)
};
cljs.core.ArrayChunk.prototype.cljs$core$IReduce$_reduce$arity$3 = function(coll, f, start) {
  var this__17520 = this;
  return cljs.core.array_reduce.call(null, this__17520.arr, f, start, this__17520.off)
};
cljs.core.ArrayChunk.prototype.cljs$core$IChunk$ = true;
cljs.core.ArrayChunk.prototype.cljs$core$IChunk$_drop_first$arity$1 = function(coll) {
  var this__17521 = this;
  if(this__17521.off === this__17521.end) {
    throw new Error("-drop-first of empty chunk");
  }else {
    return new cljs.core.ArrayChunk(this__17521.arr, this__17521.off + 1, this__17521.end)
  }
};
cljs.core.ArrayChunk.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, i) {
  var this__17522 = this;
  return this__17522.arr[this__17522.off + i]
};
cljs.core.ArrayChunk.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, i, not_found) {
  var this__17523 = this;
  if(function() {
    var and__3822__auto____17524 = i >= 0;
    if(and__3822__auto____17524) {
      return i < this__17523.end - this__17523.off
    }else {
      return and__3822__auto____17524
    }
  }()) {
    return this__17523.arr[this__17523.off + i]
  }else {
    return not_found
  }
};
cljs.core.ArrayChunk.prototype.cljs$core$ICounted$_count$arity$1 = function(_) {
  var this__17525 = this;
  return this__17525.end - this__17525.off
};
cljs.core.ArrayChunk;
cljs.core.array_chunk = function() {
  var array_chunk = null;
  var array_chunk__1 = function(arr) {
    return array_chunk.call(null, arr, 0, arr.length)
  };
  var array_chunk__2 = function(arr, off) {
    return array_chunk.call(null, arr, off, arr.length)
  };
  var array_chunk__3 = function(arr, off, end) {
    return new cljs.core.ArrayChunk(arr, off, end)
  };
  array_chunk = function(arr, off, end) {
    switch(arguments.length) {
      case 1:
        return array_chunk__1.call(this, arr);
      case 2:
        return array_chunk__2.call(this, arr, off);
      case 3:
        return array_chunk__3.call(this, arr, off, end)
    }
    throw"Invalid arity: " + arguments.length;
  };
  array_chunk.cljs$lang$arity$1 = array_chunk__1;
  array_chunk.cljs$lang$arity$2 = array_chunk__2;
  array_chunk.cljs$lang$arity$3 = array_chunk__3;
  return array_chunk
}();
goog.provide("cljs.core.ChunkedCons");
cljs.core.ChunkedCons = function(chunk, more, meta, __hash) {
  this.chunk = chunk;
  this.more = more;
  this.meta = meta;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition0$ = 31850604;
  this.cljs$lang$protocol_mask$partition1$ = 1536
};
cljs.core.ChunkedCons.cljs$lang$type = true;
cljs.core.ChunkedCons.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ChunkedCons")
};
cljs.core.ChunkedCons.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ChunkedCons")
};
cljs.core.ChunkedCons.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__17526 = this;
  var h__2247__auto____17527 = this__17526.__hash;
  if(!(h__2247__auto____17527 == null)) {
    return h__2247__auto____17527
  }else {
    var h__2247__auto____17528 = cljs.core.hash_coll.call(null, coll);
    this__17526.__hash = h__2247__auto____17528;
    return h__2247__auto____17528
  }
};
cljs.core.ChunkedCons.prototype.cljs$core$ICollection$_conj$arity$2 = function(this$, o) {
  var this__17529 = this;
  return cljs.core.cons.call(null, o, this$)
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17530 = this;
  return coll
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17531 = this;
  return cljs.core._nth.call(null, this__17531.chunk, 0)
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17532 = this;
  if(cljs.core._count.call(null, this__17532.chunk) > 1) {
    return new cljs.core.ChunkedCons(cljs.core._drop_first.call(null, this__17532.chunk), this__17532.more, this__17532.meta, null)
  }else {
    if(this__17532.more == null) {
      return cljs.core.List.EMPTY
    }else {
      return this__17532.more
    }
  }
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedNext$_chunked_next$arity$1 = function(coll) {
  var this__17533 = this;
  if(this__17533.more == null) {
    return null
  }else {
    return this__17533.more
  }
};
cljs.core.ChunkedCons.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17534 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.ChunkedCons.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, m) {
  var this__17535 = this;
  return new cljs.core.ChunkedCons(this__17535.chunk, this__17535.more, m, this__17535.__hash)
};
cljs.core.ChunkedCons.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17536 = this;
  return this__17536.meta
};
cljs.core.ChunkedCons.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17537 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17537.meta)
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedSeq$_chunked_first$arity$1 = function(coll) {
  var this__17538 = this;
  return this__17538.chunk
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedSeq$_chunked_rest$arity$1 = function(coll) {
  var this__17539 = this;
  if(this__17539.more == null) {
    return cljs.core.List.EMPTY
  }else {
    return this__17539.more
  }
};
cljs.core.ChunkedCons;
cljs.core.chunk_cons = function chunk_cons(chunk, rest) {
  if(cljs.core._count.call(null, chunk) === 0) {
    return rest
  }else {
    return new cljs.core.ChunkedCons(chunk, rest, null, null)
  }
};
cljs.core.chunk_append = function chunk_append(b, x) {
  return b.add(x)
};
cljs.core.chunk = function chunk(b) {
  return b.chunk()
};
cljs.core.chunk_first = function chunk_first(s) {
  return cljs.core._chunked_first.call(null, s)
};
cljs.core.chunk_rest = function chunk_rest(s) {
  return cljs.core._chunked_rest.call(null, s)
};
cljs.core.chunk_next = function chunk_next(s) {
  if(function() {
    var G__17543__17544 = s;
    if(G__17543__17544) {
      if(function() {
        var or__3824__auto____17545 = G__17543__17544.cljs$lang$protocol_mask$partition1$ & 1024;
        if(or__3824__auto____17545) {
          return or__3824__auto____17545
        }else {
          return G__17543__17544.cljs$core$IChunkedNext$
        }
      }()) {
        return true
      }else {
        if(!G__17543__17544.cljs$lang$protocol_mask$partition1$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedNext, G__17543__17544)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedNext, G__17543__17544)
    }
  }()) {
    return cljs.core._chunked_next.call(null, s)
  }else {
    return cljs.core.seq.call(null, cljs.core._chunked_rest.call(null, s))
  }
};
cljs.core.to_array = function to_array(s) {
  var ary__17548 = [];
  var s__17549 = s;
  while(true) {
    if(cljs.core.seq.call(null, s__17549)) {
      ary__17548.push(cljs.core.first.call(null, s__17549));
      var G__17550 = cljs.core.next.call(null, s__17549);
      s__17549 = G__17550;
      continue
    }else {
      return ary__17548
    }
    break
  }
};
cljs.core.to_array_2d = function to_array_2d(coll) {
  var ret__17554 = cljs.core.make_array.call(null, cljs.core.count.call(null, coll));
  var i__17555 = 0;
  var xs__17556 = cljs.core.seq.call(null, coll);
  while(true) {
    if(xs__17556) {
      ret__17554[i__17555] = cljs.core.to_array.call(null, cljs.core.first.call(null, xs__17556));
      var G__17557 = i__17555 + 1;
      var G__17558 = cljs.core.next.call(null, xs__17556);
      i__17555 = G__17557;
      xs__17556 = G__17558;
      continue
    }else {
    }
    break
  }
  return ret__17554
};
cljs.core.long_array = function() {
  var long_array = null;
  var long_array__1 = function(size_or_seq) {
    if(cljs.core.number_QMARK_.call(null, size_or_seq)) {
      return long_array.call(null, size_or_seq, null)
    }else {
      if(cljs.core.seq_QMARK_.call(null, size_or_seq)) {
        return cljs.core.into_array.call(null, size_or_seq)
      }else {
        if("\ufdd0'else") {
          throw new Error("long-array called with something other than size or ISeq");
        }else {
          return null
        }
      }
    }
  };
  var long_array__2 = function(size, init_val_or_seq) {
    var a__17566 = cljs.core.make_array.call(null, size);
    if(cljs.core.seq_QMARK_.call(null, init_val_or_seq)) {
      var s__17567 = cljs.core.seq.call(null, init_val_or_seq);
      var i__17568 = 0;
      var s__17569 = s__17567;
      while(true) {
        if(cljs.core.truth_(function() {
          var and__3822__auto____17570 = s__17569;
          if(and__3822__auto____17570) {
            return i__17568 < size
          }else {
            return and__3822__auto____17570
          }
        }())) {
          a__17566[i__17568] = cljs.core.first.call(null, s__17569);
          var G__17573 = i__17568 + 1;
          var G__17574 = cljs.core.next.call(null, s__17569);
          i__17568 = G__17573;
          s__17569 = G__17574;
          continue
        }else {
          return a__17566
        }
        break
      }
    }else {
      var n__2593__auto____17571 = size;
      var i__17572 = 0;
      while(true) {
        if(i__17572 < n__2593__auto____17571) {
          a__17566[i__17572] = init_val_or_seq;
          var G__17575 = i__17572 + 1;
          i__17572 = G__17575;
          continue
        }else {
        }
        break
      }
      return a__17566
    }
  };
  long_array = function(size, init_val_or_seq) {
    switch(arguments.length) {
      case 1:
        return long_array__1.call(this, size);
      case 2:
        return long_array__2.call(this, size, init_val_or_seq)
    }
    throw"Invalid arity: " + arguments.length;
  };
  long_array.cljs$lang$arity$1 = long_array__1;
  long_array.cljs$lang$arity$2 = long_array__2;
  return long_array
}();
cljs.core.double_array = function() {
  var double_array = null;
  var double_array__1 = function(size_or_seq) {
    if(cljs.core.number_QMARK_.call(null, size_or_seq)) {
      return double_array.call(null, size_or_seq, null)
    }else {
      if(cljs.core.seq_QMARK_.call(null, size_or_seq)) {
        return cljs.core.into_array.call(null, size_or_seq)
      }else {
        if("\ufdd0'else") {
          throw new Error("double-array called with something other than size or ISeq");
        }else {
          return null
        }
      }
    }
  };
  var double_array__2 = function(size, init_val_or_seq) {
    var a__17583 = cljs.core.make_array.call(null, size);
    if(cljs.core.seq_QMARK_.call(null, init_val_or_seq)) {
      var s__17584 = cljs.core.seq.call(null, init_val_or_seq);
      var i__17585 = 0;
      var s__17586 = s__17584;
      while(true) {
        if(cljs.core.truth_(function() {
          var and__3822__auto____17587 = s__17586;
          if(and__3822__auto____17587) {
            return i__17585 < size
          }else {
            return and__3822__auto____17587
          }
        }())) {
          a__17583[i__17585] = cljs.core.first.call(null, s__17586);
          var G__17590 = i__17585 + 1;
          var G__17591 = cljs.core.next.call(null, s__17586);
          i__17585 = G__17590;
          s__17586 = G__17591;
          continue
        }else {
          return a__17583
        }
        break
      }
    }else {
      var n__2593__auto____17588 = size;
      var i__17589 = 0;
      while(true) {
        if(i__17589 < n__2593__auto____17588) {
          a__17583[i__17589] = init_val_or_seq;
          var G__17592 = i__17589 + 1;
          i__17589 = G__17592;
          continue
        }else {
        }
        break
      }
      return a__17583
    }
  };
  double_array = function(size, init_val_or_seq) {
    switch(arguments.length) {
      case 1:
        return double_array__1.call(this, size);
      case 2:
        return double_array__2.call(this, size, init_val_or_seq)
    }
    throw"Invalid arity: " + arguments.length;
  };
  double_array.cljs$lang$arity$1 = double_array__1;
  double_array.cljs$lang$arity$2 = double_array__2;
  return double_array
}();
cljs.core.object_array = function() {
  var object_array = null;
  var object_array__1 = function(size_or_seq) {
    if(cljs.core.number_QMARK_.call(null, size_or_seq)) {
      return object_array.call(null, size_or_seq, null)
    }else {
      if(cljs.core.seq_QMARK_.call(null, size_or_seq)) {
        return cljs.core.into_array.call(null, size_or_seq)
      }else {
        if("\ufdd0'else") {
          throw new Error("object-array called with something other than size or ISeq");
        }else {
          return null
        }
      }
    }
  };
  var object_array__2 = function(size, init_val_or_seq) {
    var a__17600 = cljs.core.make_array.call(null, size);
    if(cljs.core.seq_QMARK_.call(null, init_val_or_seq)) {
      var s__17601 = cljs.core.seq.call(null, init_val_or_seq);
      var i__17602 = 0;
      var s__17603 = s__17601;
      while(true) {
        if(cljs.core.truth_(function() {
          var and__3822__auto____17604 = s__17603;
          if(and__3822__auto____17604) {
            return i__17602 < size
          }else {
            return and__3822__auto____17604
          }
        }())) {
          a__17600[i__17602] = cljs.core.first.call(null, s__17603);
          var G__17607 = i__17602 + 1;
          var G__17608 = cljs.core.next.call(null, s__17603);
          i__17602 = G__17607;
          s__17603 = G__17608;
          continue
        }else {
          return a__17600
        }
        break
      }
    }else {
      var n__2593__auto____17605 = size;
      var i__17606 = 0;
      while(true) {
        if(i__17606 < n__2593__auto____17605) {
          a__17600[i__17606] = init_val_or_seq;
          var G__17609 = i__17606 + 1;
          i__17606 = G__17609;
          continue
        }else {
        }
        break
      }
      return a__17600
    }
  };
  object_array = function(size, init_val_or_seq) {
    switch(arguments.length) {
      case 1:
        return object_array__1.call(this, size);
      case 2:
        return object_array__2.call(this, size, init_val_or_seq)
    }
    throw"Invalid arity: " + arguments.length;
  };
  object_array.cljs$lang$arity$1 = object_array__1;
  object_array.cljs$lang$arity$2 = object_array__2;
  return object_array
}();
cljs.core.bounded_count = function bounded_count(s, n) {
  if(cljs.core.counted_QMARK_.call(null, s)) {
    return cljs.core.count.call(null, s)
  }else {
    var s__17614 = s;
    var i__17615 = n;
    var sum__17616 = 0;
    while(true) {
      if(cljs.core.truth_(function() {
        var and__3822__auto____17617 = i__17615 > 0;
        if(and__3822__auto____17617) {
          return cljs.core.seq.call(null, s__17614)
        }else {
          return and__3822__auto____17617
        }
      }())) {
        var G__17618 = cljs.core.next.call(null, s__17614);
        var G__17619 = i__17615 - 1;
        var G__17620 = sum__17616 + 1;
        s__17614 = G__17618;
        i__17615 = G__17619;
        sum__17616 = G__17620;
        continue
      }else {
        return sum__17616
      }
      break
    }
  }
};
cljs.core.spread = function spread(arglist) {
  if(arglist == null) {
    return null
  }else {
    if(cljs.core.next.call(null, arglist) == null) {
      return cljs.core.seq.call(null, cljs.core.first.call(null, arglist))
    }else {
      if("\ufdd0'else") {
        return cljs.core.cons.call(null, cljs.core.first.call(null, arglist), spread.call(null, cljs.core.next.call(null, arglist)))
      }else {
        return null
      }
    }
  }
};
cljs.core.concat = function() {
  var concat = null;
  var concat__0 = function() {
    return new cljs.core.LazySeq(null, false, function() {
      return null
    }, null)
  };
  var concat__1 = function(x) {
    return new cljs.core.LazySeq(null, false, function() {
      return x
    }, null)
  };
  var concat__2 = function(x, y) {
    return new cljs.core.LazySeq(null, false, function() {
      var s__17625 = cljs.core.seq.call(null, x);
      if(s__17625) {
        if(cljs.core.chunked_seq_QMARK_.call(null, s__17625)) {
          return cljs.core.chunk_cons.call(null, cljs.core.chunk_first.call(null, s__17625), concat.call(null, cljs.core.chunk_rest.call(null, s__17625), y))
        }else {
          return cljs.core.cons.call(null, cljs.core.first.call(null, s__17625), concat.call(null, cljs.core.rest.call(null, s__17625), y))
        }
      }else {
        return y
      }
    }, null)
  };
  var concat__3 = function() {
    var G__17629__delegate = function(x, y, zs) {
      var cat__17628 = function cat(xys, zs) {
        return new cljs.core.LazySeq(null, false, function() {
          var xys__17627 = cljs.core.seq.call(null, xys);
          if(xys__17627) {
            if(cljs.core.chunked_seq_QMARK_.call(null, xys__17627)) {
              return cljs.core.chunk_cons.call(null, cljs.core.chunk_first.call(null, xys__17627), cat.call(null, cljs.core.chunk_rest.call(null, xys__17627), zs))
            }else {
              return cljs.core.cons.call(null, cljs.core.first.call(null, xys__17627), cat.call(null, cljs.core.rest.call(null, xys__17627), zs))
            }
          }else {
            if(cljs.core.truth_(zs)) {
              return cat.call(null, cljs.core.first.call(null, zs), cljs.core.next.call(null, zs))
            }else {
              return null
            }
          }
        }, null)
      };
      return cat__17628.call(null, concat.call(null, x, y), zs)
    };
    var G__17629 = function(x, y, var_args) {
      var zs = null;
      if(goog.isDef(var_args)) {
        zs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17629__delegate.call(this, x, y, zs)
    };
    G__17629.cljs$lang$maxFixedArity = 2;
    G__17629.cljs$lang$applyTo = function(arglist__17630) {
      var x = cljs.core.first(arglist__17630);
      var y = cljs.core.first(cljs.core.next(arglist__17630));
      var zs = cljs.core.rest(cljs.core.next(arglist__17630));
      return G__17629__delegate(x, y, zs)
    };
    G__17629.cljs$lang$arity$variadic = G__17629__delegate;
    return G__17629
  }();
  concat = function(x, y, var_args) {
    var zs = var_args;
    switch(arguments.length) {
      case 0:
        return concat__0.call(this);
      case 1:
        return concat__1.call(this, x);
      case 2:
        return concat__2.call(this, x, y);
      default:
        return concat__3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  concat.cljs$lang$maxFixedArity = 2;
  concat.cljs$lang$applyTo = concat__3.cljs$lang$applyTo;
  concat.cljs$lang$arity$0 = concat__0;
  concat.cljs$lang$arity$1 = concat__1;
  concat.cljs$lang$arity$2 = concat__2;
  concat.cljs$lang$arity$variadic = concat__3.cljs$lang$arity$variadic;
  return concat
}();
cljs.core.list_STAR_ = function() {
  var list_STAR_ = null;
  var list_STAR___1 = function(args) {
    return cljs.core.seq.call(null, args)
  };
  var list_STAR___2 = function(a, args) {
    return cljs.core.cons.call(null, a, args)
  };
  var list_STAR___3 = function(a, b, args) {
    return cljs.core.cons.call(null, a, cljs.core.cons.call(null, b, args))
  };
  var list_STAR___4 = function(a, b, c, args) {
    return cljs.core.cons.call(null, a, cljs.core.cons.call(null, b, cljs.core.cons.call(null, c, args)))
  };
  var list_STAR___5 = function() {
    var G__17631__delegate = function(a, b, c, d, more) {
      return cljs.core.cons.call(null, a, cljs.core.cons.call(null, b, cljs.core.cons.call(null, c, cljs.core.cons.call(null, d, cljs.core.spread.call(null, more)))))
    };
    var G__17631 = function(a, b, c, d, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__17631__delegate.call(this, a, b, c, d, more)
    };
    G__17631.cljs$lang$maxFixedArity = 4;
    G__17631.cljs$lang$applyTo = function(arglist__17632) {
      var a = cljs.core.first(arglist__17632);
      var b = cljs.core.first(cljs.core.next(arglist__17632));
      var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17632)));
      var d = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17632))));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17632))));
      return G__17631__delegate(a, b, c, d, more)
    };
    G__17631.cljs$lang$arity$variadic = G__17631__delegate;
    return G__17631
  }();
  list_STAR_ = function(a, b, c, d, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return list_STAR___1.call(this, a);
      case 2:
        return list_STAR___2.call(this, a, b);
      case 3:
        return list_STAR___3.call(this, a, b, c);
      case 4:
        return list_STAR___4.call(this, a, b, c, d);
      default:
        return list_STAR___5.cljs$lang$arity$variadic(a, b, c, d, cljs.core.array_seq(arguments, 4))
    }
    throw"Invalid arity: " + arguments.length;
  };
  list_STAR_.cljs$lang$maxFixedArity = 4;
  list_STAR_.cljs$lang$applyTo = list_STAR___5.cljs$lang$applyTo;
  list_STAR_.cljs$lang$arity$1 = list_STAR___1;
  list_STAR_.cljs$lang$arity$2 = list_STAR___2;
  list_STAR_.cljs$lang$arity$3 = list_STAR___3;
  list_STAR_.cljs$lang$arity$4 = list_STAR___4;
  list_STAR_.cljs$lang$arity$variadic = list_STAR___5.cljs$lang$arity$variadic;
  return list_STAR_
}();
cljs.core.transient$ = function transient$(coll) {
  return cljs.core._as_transient.call(null, coll)
};
cljs.core.persistent_BANG_ = function persistent_BANG_(tcoll) {
  return cljs.core._persistent_BANG_.call(null, tcoll)
};
cljs.core.conj_BANG_ = function conj_BANG_(tcoll, val) {
  return cljs.core._conj_BANG_.call(null, tcoll, val)
};
cljs.core.assoc_BANG_ = function assoc_BANG_(tcoll, key, val) {
  return cljs.core._assoc_BANG_.call(null, tcoll, key, val)
};
cljs.core.dissoc_BANG_ = function dissoc_BANG_(tcoll, key) {
  return cljs.core._dissoc_BANG_.call(null, tcoll, key)
};
cljs.core.pop_BANG_ = function pop_BANG_(tcoll) {
  return cljs.core._pop_BANG_.call(null, tcoll)
};
cljs.core.disj_BANG_ = function disj_BANG_(tcoll, val) {
  return cljs.core._disjoin_BANG_.call(null, tcoll, val)
};
cljs.core.apply_to = function apply_to(f, argc, args) {
  var args__17674 = cljs.core.seq.call(null, args);
  if(argc === 0) {
    return f.call(null)
  }else {
    var a__17675 = cljs.core._first.call(null, args__17674);
    var args__17676 = cljs.core._rest.call(null, args__17674);
    if(argc === 1) {
      if(f.cljs$lang$arity$1) {
        return f.cljs$lang$arity$1(a__17675)
      }else {
        return f.call(null, a__17675)
      }
    }else {
      var b__17677 = cljs.core._first.call(null, args__17676);
      var args__17678 = cljs.core._rest.call(null, args__17676);
      if(argc === 2) {
        if(f.cljs$lang$arity$2) {
          return f.cljs$lang$arity$2(a__17675, b__17677)
        }else {
          return f.call(null, a__17675, b__17677)
        }
      }else {
        var c__17679 = cljs.core._first.call(null, args__17678);
        var args__17680 = cljs.core._rest.call(null, args__17678);
        if(argc === 3) {
          if(f.cljs$lang$arity$3) {
            return f.cljs$lang$arity$3(a__17675, b__17677, c__17679)
          }else {
            return f.call(null, a__17675, b__17677, c__17679)
          }
        }else {
          var d__17681 = cljs.core._first.call(null, args__17680);
          var args__17682 = cljs.core._rest.call(null, args__17680);
          if(argc === 4) {
            if(f.cljs$lang$arity$4) {
              return f.cljs$lang$arity$4(a__17675, b__17677, c__17679, d__17681)
            }else {
              return f.call(null, a__17675, b__17677, c__17679, d__17681)
            }
          }else {
            var e__17683 = cljs.core._first.call(null, args__17682);
            var args__17684 = cljs.core._rest.call(null, args__17682);
            if(argc === 5) {
              if(f.cljs$lang$arity$5) {
                return f.cljs$lang$arity$5(a__17675, b__17677, c__17679, d__17681, e__17683)
              }else {
                return f.call(null, a__17675, b__17677, c__17679, d__17681, e__17683)
              }
            }else {
              var f__17685 = cljs.core._first.call(null, args__17684);
              var args__17686 = cljs.core._rest.call(null, args__17684);
              if(argc === 6) {
                if(f__17685.cljs$lang$arity$6) {
                  return f__17685.cljs$lang$arity$6(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685)
                }else {
                  return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685)
                }
              }else {
                var g__17687 = cljs.core._first.call(null, args__17686);
                var args__17688 = cljs.core._rest.call(null, args__17686);
                if(argc === 7) {
                  if(f__17685.cljs$lang$arity$7) {
                    return f__17685.cljs$lang$arity$7(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687)
                  }else {
                    return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687)
                  }
                }else {
                  var h__17689 = cljs.core._first.call(null, args__17688);
                  var args__17690 = cljs.core._rest.call(null, args__17688);
                  if(argc === 8) {
                    if(f__17685.cljs$lang$arity$8) {
                      return f__17685.cljs$lang$arity$8(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689)
                    }else {
                      return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689)
                    }
                  }else {
                    var i__17691 = cljs.core._first.call(null, args__17690);
                    var args__17692 = cljs.core._rest.call(null, args__17690);
                    if(argc === 9) {
                      if(f__17685.cljs$lang$arity$9) {
                        return f__17685.cljs$lang$arity$9(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691)
                      }else {
                        return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691)
                      }
                    }else {
                      var j__17693 = cljs.core._first.call(null, args__17692);
                      var args__17694 = cljs.core._rest.call(null, args__17692);
                      if(argc === 10) {
                        if(f__17685.cljs$lang$arity$10) {
                          return f__17685.cljs$lang$arity$10(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693)
                        }else {
                          return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693)
                        }
                      }else {
                        var k__17695 = cljs.core._first.call(null, args__17694);
                        var args__17696 = cljs.core._rest.call(null, args__17694);
                        if(argc === 11) {
                          if(f__17685.cljs$lang$arity$11) {
                            return f__17685.cljs$lang$arity$11(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695)
                          }else {
                            return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695)
                          }
                        }else {
                          var l__17697 = cljs.core._first.call(null, args__17696);
                          var args__17698 = cljs.core._rest.call(null, args__17696);
                          if(argc === 12) {
                            if(f__17685.cljs$lang$arity$12) {
                              return f__17685.cljs$lang$arity$12(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697)
                            }else {
                              return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697)
                            }
                          }else {
                            var m__17699 = cljs.core._first.call(null, args__17698);
                            var args__17700 = cljs.core._rest.call(null, args__17698);
                            if(argc === 13) {
                              if(f__17685.cljs$lang$arity$13) {
                                return f__17685.cljs$lang$arity$13(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699)
                              }else {
                                return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699)
                              }
                            }else {
                              var n__17701 = cljs.core._first.call(null, args__17700);
                              var args__17702 = cljs.core._rest.call(null, args__17700);
                              if(argc === 14) {
                                if(f__17685.cljs$lang$arity$14) {
                                  return f__17685.cljs$lang$arity$14(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701)
                                }else {
                                  return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701)
                                }
                              }else {
                                var o__17703 = cljs.core._first.call(null, args__17702);
                                var args__17704 = cljs.core._rest.call(null, args__17702);
                                if(argc === 15) {
                                  if(f__17685.cljs$lang$arity$15) {
                                    return f__17685.cljs$lang$arity$15(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703)
                                  }else {
                                    return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703)
                                  }
                                }else {
                                  var p__17705 = cljs.core._first.call(null, args__17704);
                                  var args__17706 = cljs.core._rest.call(null, args__17704);
                                  if(argc === 16) {
                                    if(f__17685.cljs$lang$arity$16) {
                                      return f__17685.cljs$lang$arity$16(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705)
                                    }else {
                                      return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705)
                                    }
                                  }else {
                                    var q__17707 = cljs.core._first.call(null, args__17706);
                                    var args__17708 = cljs.core._rest.call(null, args__17706);
                                    if(argc === 17) {
                                      if(f__17685.cljs$lang$arity$17) {
                                        return f__17685.cljs$lang$arity$17(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707)
                                      }else {
                                        return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707)
                                      }
                                    }else {
                                      var r__17709 = cljs.core._first.call(null, args__17708);
                                      var args__17710 = cljs.core._rest.call(null, args__17708);
                                      if(argc === 18) {
                                        if(f__17685.cljs$lang$arity$18) {
                                          return f__17685.cljs$lang$arity$18(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707, r__17709)
                                        }else {
                                          return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707, r__17709)
                                        }
                                      }else {
                                        var s__17711 = cljs.core._first.call(null, args__17710);
                                        var args__17712 = cljs.core._rest.call(null, args__17710);
                                        if(argc === 19) {
                                          if(f__17685.cljs$lang$arity$19) {
                                            return f__17685.cljs$lang$arity$19(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707, r__17709, s__17711)
                                          }else {
                                            return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707, r__17709, s__17711)
                                          }
                                        }else {
                                          var t__17713 = cljs.core._first.call(null, args__17712);
                                          var args__17714 = cljs.core._rest.call(null, args__17712);
                                          if(argc === 20) {
                                            if(f__17685.cljs$lang$arity$20) {
                                              return f__17685.cljs$lang$arity$20(a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707, r__17709, s__17711, t__17713)
                                            }else {
                                              return f__17685.call(null, a__17675, b__17677, c__17679, d__17681, e__17683, f__17685, g__17687, h__17689, i__17691, j__17693, k__17695, l__17697, m__17699, n__17701, o__17703, p__17705, q__17707, r__17709, s__17711, t__17713)
                                            }
                                          }else {
                                            throw new Error("Only up to 20 arguments supported on functions");
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};
cljs.core.apply = function() {
  var apply = null;
  var apply__2 = function(f, args) {
    var fixed_arity__17729 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__17730 = cljs.core.bounded_count.call(null, args, fixed_arity__17729 + 1);
      if(bc__17730 <= fixed_arity__17729) {
        return cljs.core.apply_to.call(null, f, bc__17730, args)
      }else {
        return f.cljs$lang$applyTo(args)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, args))
    }
  };
  var apply__3 = function(f, x, args) {
    var arglist__17731 = cljs.core.list_STAR_.call(null, x, args);
    var fixed_arity__17732 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__17733 = cljs.core.bounded_count.call(null, arglist__17731, fixed_arity__17732 + 1);
      if(bc__17733 <= fixed_arity__17732) {
        return cljs.core.apply_to.call(null, f, bc__17733, arglist__17731)
      }else {
        return f.cljs$lang$applyTo(arglist__17731)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, arglist__17731))
    }
  };
  var apply__4 = function(f, x, y, args) {
    var arglist__17734 = cljs.core.list_STAR_.call(null, x, y, args);
    var fixed_arity__17735 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__17736 = cljs.core.bounded_count.call(null, arglist__17734, fixed_arity__17735 + 1);
      if(bc__17736 <= fixed_arity__17735) {
        return cljs.core.apply_to.call(null, f, bc__17736, arglist__17734)
      }else {
        return f.cljs$lang$applyTo(arglist__17734)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, arglist__17734))
    }
  };
  var apply__5 = function(f, x, y, z, args) {
    var arglist__17737 = cljs.core.list_STAR_.call(null, x, y, z, args);
    var fixed_arity__17738 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__17739 = cljs.core.bounded_count.call(null, arglist__17737, fixed_arity__17738 + 1);
      if(bc__17739 <= fixed_arity__17738) {
        return cljs.core.apply_to.call(null, f, bc__17739, arglist__17737)
      }else {
        return f.cljs$lang$applyTo(arglist__17737)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, arglist__17737))
    }
  };
  var apply__6 = function() {
    var G__17743__delegate = function(f, a, b, c, d, args) {
      var arglist__17740 = cljs.core.cons.call(null, a, cljs.core.cons.call(null, b, cljs.core.cons.call(null, c, cljs.core.cons.call(null, d, cljs.core.spread.call(null, args)))));
      var fixed_arity__17741 = f.cljs$lang$maxFixedArity;
      if(f.cljs$lang$applyTo) {
        var bc__17742 = cljs.core.bounded_count.call(null, arglist__17740, fixed_arity__17741 + 1);
        if(bc__17742 <= fixed_arity__17741) {
          return cljs.core.apply_to.call(null, f, bc__17742, arglist__17740)
        }else {
          return f.cljs$lang$applyTo(arglist__17740)
        }
      }else {
        return f.apply(f, cljs.core.to_array.call(null, arglist__17740))
      }
    };
    var G__17743 = function(f, a, b, c, d, var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 5), 0)
      }
      return G__17743__delegate.call(this, f, a, b, c, d, args)
    };
    G__17743.cljs$lang$maxFixedArity = 5;
    G__17743.cljs$lang$applyTo = function(arglist__17744) {
      var f = cljs.core.first(arglist__17744);
      var a = cljs.core.first(cljs.core.next(arglist__17744));
      var b = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17744)));
      var c = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17744))));
      var d = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17744)))));
      var args = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17744)))));
      return G__17743__delegate(f, a, b, c, d, args)
    };
    G__17743.cljs$lang$arity$variadic = G__17743__delegate;
    return G__17743
  }();
  apply = function(f, a, b, c, d, var_args) {
    var args = var_args;
    switch(arguments.length) {
      case 2:
        return apply__2.call(this, f, a);
      case 3:
        return apply__3.call(this, f, a, b);
      case 4:
        return apply__4.call(this, f, a, b, c);
      case 5:
        return apply__5.call(this, f, a, b, c, d);
      default:
        return apply__6.cljs$lang$arity$variadic(f, a, b, c, d, cljs.core.array_seq(arguments, 5))
    }
    throw"Invalid arity: " + arguments.length;
  };
  apply.cljs$lang$maxFixedArity = 5;
  apply.cljs$lang$applyTo = apply__6.cljs$lang$applyTo;
  apply.cljs$lang$arity$2 = apply__2;
  apply.cljs$lang$arity$3 = apply__3;
  apply.cljs$lang$arity$4 = apply__4;
  apply.cljs$lang$arity$5 = apply__5;
  apply.cljs$lang$arity$variadic = apply__6.cljs$lang$arity$variadic;
  return apply
}();
cljs.core.vary_meta = function() {
  var vary_meta__delegate = function(obj, f, args) {
    return cljs.core.with_meta.call(null, obj, cljs.core.apply.call(null, f, cljs.core.meta.call(null, obj), args))
  };
  var vary_meta = function(obj, f, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return vary_meta__delegate.call(this, obj, f, args)
  };
  vary_meta.cljs$lang$maxFixedArity = 2;
  vary_meta.cljs$lang$applyTo = function(arglist__17745) {
    var obj = cljs.core.first(arglist__17745);
    var f = cljs.core.first(cljs.core.next(arglist__17745));
    var args = cljs.core.rest(cljs.core.next(arglist__17745));
    return vary_meta__delegate(obj, f, args)
  };
  vary_meta.cljs$lang$arity$variadic = vary_meta__delegate;
  return vary_meta
}();
cljs.core.not_EQ_ = function() {
  var not_EQ_ = null;
  var not_EQ___1 = function(x) {
    return false
  };
  var not_EQ___2 = function(x, y) {
    return!cljs.core._EQ_.call(null, x, y)
  };
  var not_EQ___3 = function() {
    var G__17746__delegate = function(x, y, more) {
      return cljs.core.not.call(null, cljs.core.apply.call(null, cljs.core._EQ_, x, y, more))
    };
    var G__17746 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17746__delegate.call(this, x, y, more)
    };
    G__17746.cljs$lang$maxFixedArity = 2;
    G__17746.cljs$lang$applyTo = function(arglist__17747) {
      var x = cljs.core.first(arglist__17747);
      var y = cljs.core.first(cljs.core.next(arglist__17747));
      var more = cljs.core.rest(cljs.core.next(arglist__17747));
      return G__17746__delegate(x, y, more)
    };
    G__17746.cljs$lang$arity$variadic = G__17746__delegate;
    return G__17746
  }();
  not_EQ_ = function(x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 1:
        return not_EQ___1.call(this, x);
      case 2:
        return not_EQ___2.call(this, x, y);
      default:
        return not_EQ___3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  not_EQ_.cljs$lang$maxFixedArity = 2;
  not_EQ_.cljs$lang$applyTo = not_EQ___3.cljs$lang$applyTo;
  not_EQ_.cljs$lang$arity$1 = not_EQ___1;
  not_EQ_.cljs$lang$arity$2 = not_EQ___2;
  not_EQ_.cljs$lang$arity$variadic = not_EQ___3.cljs$lang$arity$variadic;
  return not_EQ_
}();
cljs.core.not_empty = function not_empty(coll) {
  if(cljs.core.seq.call(null, coll)) {
    return coll
  }else {
    return null
  }
};
cljs.core.every_QMARK_ = function every_QMARK_(pred, coll) {
  while(true) {
    if(cljs.core.seq.call(null, coll) == null) {
      return true
    }else {
      if(cljs.core.truth_(pred.call(null, cljs.core.first.call(null, coll)))) {
        var G__17748 = pred;
        var G__17749 = cljs.core.next.call(null, coll);
        pred = G__17748;
        coll = G__17749;
        continue
      }else {
        if("\ufdd0'else") {
          return false
        }else {
          return null
        }
      }
    }
    break
  }
};
cljs.core.not_every_QMARK_ = function not_every_QMARK_(pred, coll) {
  return!cljs.core.every_QMARK_.call(null, pred, coll)
};
cljs.core.some = function some(pred, coll) {
  while(true) {
    if(cljs.core.seq.call(null, coll)) {
      var or__3824__auto____17751 = pred.call(null, cljs.core.first.call(null, coll));
      if(cljs.core.truth_(or__3824__auto____17751)) {
        return or__3824__auto____17751
      }else {
        var G__17752 = pred;
        var G__17753 = cljs.core.next.call(null, coll);
        pred = G__17752;
        coll = G__17753;
        continue
      }
    }else {
      return null
    }
    break
  }
};
cljs.core.not_any_QMARK_ = function not_any_QMARK_(pred, coll) {
  return cljs.core.not.call(null, cljs.core.some.call(null, pred, coll))
};
cljs.core.even_QMARK_ = function even_QMARK_(n) {
  if(cljs.core.integer_QMARK_.call(null, n)) {
    return(n & 1) === 0
  }else {
    throw new Error([cljs.core.str("Argument must be an integer: "), cljs.core.str(n)].join(""));
  }
};
cljs.core.odd_QMARK_ = function odd_QMARK_(n) {
  return!cljs.core.even_QMARK_.call(null, n)
};
cljs.core.identity = function identity(x) {
  return x
};
cljs.core.complement = function complement(f) {
  return function() {
    var G__17754 = null;
    var G__17754__0 = function() {
      return cljs.core.not.call(null, f.call(null))
    };
    var G__17754__1 = function(x) {
      return cljs.core.not.call(null, f.call(null, x))
    };
    var G__17754__2 = function(x, y) {
      return cljs.core.not.call(null, f.call(null, x, y))
    };
    var G__17754__3 = function() {
      var G__17755__delegate = function(x, y, zs) {
        return cljs.core.not.call(null, cljs.core.apply.call(null, f, x, y, zs))
      };
      var G__17755 = function(x, y, var_args) {
        var zs = null;
        if(goog.isDef(var_args)) {
          zs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
        }
        return G__17755__delegate.call(this, x, y, zs)
      };
      G__17755.cljs$lang$maxFixedArity = 2;
      G__17755.cljs$lang$applyTo = function(arglist__17756) {
        var x = cljs.core.first(arglist__17756);
        var y = cljs.core.first(cljs.core.next(arglist__17756));
        var zs = cljs.core.rest(cljs.core.next(arglist__17756));
        return G__17755__delegate(x, y, zs)
      };
      G__17755.cljs$lang$arity$variadic = G__17755__delegate;
      return G__17755
    }();
    G__17754 = function(x, y, var_args) {
      var zs = var_args;
      switch(arguments.length) {
        case 0:
          return G__17754__0.call(this);
        case 1:
          return G__17754__1.call(this, x);
        case 2:
          return G__17754__2.call(this, x, y);
        default:
          return G__17754__3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
      }
      throw"Invalid arity: " + arguments.length;
    };
    G__17754.cljs$lang$maxFixedArity = 2;
    G__17754.cljs$lang$applyTo = G__17754__3.cljs$lang$applyTo;
    return G__17754
  }()
};
cljs.core.constantly = function constantly(x) {
  return function() {
    var G__17757__delegate = function(args) {
      return x
    };
    var G__17757 = function(var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__17757__delegate.call(this, args)
    };
    G__17757.cljs$lang$maxFixedArity = 0;
    G__17757.cljs$lang$applyTo = function(arglist__17758) {
      var args = cljs.core.seq(arglist__17758);
      return G__17757__delegate(args)
    };
    G__17757.cljs$lang$arity$variadic = G__17757__delegate;
    return G__17757
  }()
};
cljs.core.comp = function() {
  var comp = null;
  var comp__0 = function() {
    return cljs.core.identity
  };
  var comp__1 = function(f) {
    return f
  };
  var comp__2 = function(f, g) {
    return function() {
      var G__17765 = null;
      var G__17765__0 = function() {
        return f.call(null, g.call(null))
      };
      var G__17765__1 = function(x) {
        return f.call(null, g.call(null, x))
      };
      var G__17765__2 = function(x, y) {
        return f.call(null, g.call(null, x, y))
      };
      var G__17765__3 = function(x, y, z) {
        return f.call(null, g.call(null, x, y, z))
      };
      var G__17765__4 = function() {
        var G__17766__delegate = function(x, y, z, args) {
          return f.call(null, cljs.core.apply.call(null, g, x, y, z, args))
        };
        var G__17766 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__17766__delegate.call(this, x, y, z, args)
        };
        G__17766.cljs$lang$maxFixedArity = 3;
        G__17766.cljs$lang$applyTo = function(arglist__17767) {
          var x = cljs.core.first(arglist__17767);
          var y = cljs.core.first(cljs.core.next(arglist__17767));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17767)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17767)));
          return G__17766__delegate(x, y, z, args)
        };
        G__17766.cljs$lang$arity$variadic = G__17766__delegate;
        return G__17766
      }();
      G__17765 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__17765__0.call(this);
          case 1:
            return G__17765__1.call(this, x);
          case 2:
            return G__17765__2.call(this, x, y);
          case 3:
            return G__17765__3.call(this, x, y, z);
          default:
            return G__17765__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__17765.cljs$lang$maxFixedArity = 3;
      G__17765.cljs$lang$applyTo = G__17765__4.cljs$lang$applyTo;
      return G__17765
    }()
  };
  var comp__3 = function(f, g, h) {
    return function() {
      var G__17768 = null;
      var G__17768__0 = function() {
        return f.call(null, g.call(null, h.call(null)))
      };
      var G__17768__1 = function(x) {
        return f.call(null, g.call(null, h.call(null, x)))
      };
      var G__17768__2 = function(x, y) {
        return f.call(null, g.call(null, h.call(null, x, y)))
      };
      var G__17768__3 = function(x, y, z) {
        return f.call(null, g.call(null, h.call(null, x, y, z)))
      };
      var G__17768__4 = function() {
        var G__17769__delegate = function(x, y, z, args) {
          return f.call(null, g.call(null, cljs.core.apply.call(null, h, x, y, z, args)))
        };
        var G__17769 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__17769__delegate.call(this, x, y, z, args)
        };
        G__17769.cljs$lang$maxFixedArity = 3;
        G__17769.cljs$lang$applyTo = function(arglist__17770) {
          var x = cljs.core.first(arglist__17770);
          var y = cljs.core.first(cljs.core.next(arglist__17770));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17770)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17770)));
          return G__17769__delegate(x, y, z, args)
        };
        G__17769.cljs$lang$arity$variadic = G__17769__delegate;
        return G__17769
      }();
      G__17768 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__17768__0.call(this);
          case 1:
            return G__17768__1.call(this, x);
          case 2:
            return G__17768__2.call(this, x, y);
          case 3:
            return G__17768__3.call(this, x, y, z);
          default:
            return G__17768__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__17768.cljs$lang$maxFixedArity = 3;
      G__17768.cljs$lang$applyTo = G__17768__4.cljs$lang$applyTo;
      return G__17768
    }()
  };
  var comp__4 = function() {
    var G__17771__delegate = function(f1, f2, f3, fs) {
      var fs__17762 = cljs.core.reverse.call(null, cljs.core.list_STAR_.call(null, f1, f2, f3, fs));
      return function() {
        var G__17772__delegate = function(args) {
          var ret__17763 = cljs.core.apply.call(null, cljs.core.first.call(null, fs__17762), args);
          var fs__17764 = cljs.core.next.call(null, fs__17762);
          while(true) {
            if(fs__17764) {
              var G__17773 = cljs.core.first.call(null, fs__17764).call(null, ret__17763);
              var G__17774 = cljs.core.next.call(null, fs__17764);
              ret__17763 = G__17773;
              fs__17764 = G__17774;
              continue
            }else {
              return ret__17763
            }
            break
          }
        };
        var G__17772 = function(var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
          }
          return G__17772__delegate.call(this, args)
        };
        G__17772.cljs$lang$maxFixedArity = 0;
        G__17772.cljs$lang$applyTo = function(arglist__17775) {
          var args = cljs.core.seq(arglist__17775);
          return G__17772__delegate(args)
        };
        G__17772.cljs$lang$arity$variadic = G__17772__delegate;
        return G__17772
      }()
    };
    var G__17771 = function(f1, f2, f3, var_args) {
      var fs = null;
      if(goog.isDef(var_args)) {
        fs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__17771__delegate.call(this, f1, f2, f3, fs)
    };
    G__17771.cljs$lang$maxFixedArity = 3;
    G__17771.cljs$lang$applyTo = function(arglist__17776) {
      var f1 = cljs.core.first(arglist__17776);
      var f2 = cljs.core.first(cljs.core.next(arglist__17776));
      var f3 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17776)));
      var fs = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17776)));
      return G__17771__delegate(f1, f2, f3, fs)
    };
    G__17771.cljs$lang$arity$variadic = G__17771__delegate;
    return G__17771
  }();
  comp = function(f1, f2, f3, var_args) {
    var fs = var_args;
    switch(arguments.length) {
      case 0:
        return comp__0.call(this);
      case 1:
        return comp__1.call(this, f1);
      case 2:
        return comp__2.call(this, f1, f2);
      case 3:
        return comp__3.call(this, f1, f2, f3);
      default:
        return comp__4.cljs$lang$arity$variadic(f1, f2, f3, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  comp.cljs$lang$maxFixedArity = 3;
  comp.cljs$lang$applyTo = comp__4.cljs$lang$applyTo;
  comp.cljs$lang$arity$0 = comp__0;
  comp.cljs$lang$arity$1 = comp__1;
  comp.cljs$lang$arity$2 = comp__2;
  comp.cljs$lang$arity$3 = comp__3;
  comp.cljs$lang$arity$variadic = comp__4.cljs$lang$arity$variadic;
  return comp
}();
cljs.core.partial = function() {
  var partial = null;
  var partial__2 = function(f, arg1) {
    return function() {
      var G__17777__delegate = function(args) {
        return cljs.core.apply.call(null, f, arg1, args)
      };
      var G__17777 = function(var_args) {
        var args = null;
        if(goog.isDef(var_args)) {
          args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
        }
        return G__17777__delegate.call(this, args)
      };
      G__17777.cljs$lang$maxFixedArity = 0;
      G__17777.cljs$lang$applyTo = function(arglist__17778) {
        var args = cljs.core.seq(arglist__17778);
        return G__17777__delegate(args)
      };
      G__17777.cljs$lang$arity$variadic = G__17777__delegate;
      return G__17777
    }()
  };
  var partial__3 = function(f, arg1, arg2) {
    return function() {
      var G__17779__delegate = function(args) {
        return cljs.core.apply.call(null, f, arg1, arg2, args)
      };
      var G__17779 = function(var_args) {
        var args = null;
        if(goog.isDef(var_args)) {
          args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
        }
        return G__17779__delegate.call(this, args)
      };
      G__17779.cljs$lang$maxFixedArity = 0;
      G__17779.cljs$lang$applyTo = function(arglist__17780) {
        var args = cljs.core.seq(arglist__17780);
        return G__17779__delegate(args)
      };
      G__17779.cljs$lang$arity$variadic = G__17779__delegate;
      return G__17779
    }()
  };
  var partial__4 = function(f, arg1, arg2, arg3) {
    return function() {
      var G__17781__delegate = function(args) {
        return cljs.core.apply.call(null, f, arg1, arg2, arg3, args)
      };
      var G__17781 = function(var_args) {
        var args = null;
        if(goog.isDef(var_args)) {
          args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
        }
        return G__17781__delegate.call(this, args)
      };
      G__17781.cljs$lang$maxFixedArity = 0;
      G__17781.cljs$lang$applyTo = function(arglist__17782) {
        var args = cljs.core.seq(arglist__17782);
        return G__17781__delegate(args)
      };
      G__17781.cljs$lang$arity$variadic = G__17781__delegate;
      return G__17781
    }()
  };
  var partial__5 = function() {
    var G__17783__delegate = function(f, arg1, arg2, arg3, more) {
      return function() {
        var G__17784__delegate = function(args) {
          return cljs.core.apply.call(null, f, arg1, arg2, arg3, cljs.core.concat.call(null, more, args))
        };
        var G__17784 = function(var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
          }
          return G__17784__delegate.call(this, args)
        };
        G__17784.cljs$lang$maxFixedArity = 0;
        G__17784.cljs$lang$applyTo = function(arglist__17785) {
          var args = cljs.core.seq(arglist__17785);
          return G__17784__delegate(args)
        };
        G__17784.cljs$lang$arity$variadic = G__17784__delegate;
        return G__17784
      }()
    };
    var G__17783 = function(f, arg1, arg2, arg3, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__17783__delegate.call(this, f, arg1, arg2, arg3, more)
    };
    G__17783.cljs$lang$maxFixedArity = 4;
    G__17783.cljs$lang$applyTo = function(arglist__17786) {
      var f = cljs.core.first(arglist__17786);
      var arg1 = cljs.core.first(cljs.core.next(arglist__17786));
      var arg2 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17786)));
      var arg3 = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17786))));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__17786))));
      return G__17783__delegate(f, arg1, arg2, arg3, more)
    };
    G__17783.cljs$lang$arity$variadic = G__17783__delegate;
    return G__17783
  }();
  partial = function(f, arg1, arg2, arg3, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 2:
        return partial__2.call(this, f, arg1);
      case 3:
        return partial__3.call(this, f, arg1, arg2);
      case 4:
        return partial__4.call(this, f, arg1, arg2, arg3);
      default:
        return partial__5.cljs$lang$arity$variadic(f, arg1, arg2, arg3, cljs.core.array_seq(arguments, 4))
    }
    throw"Invalid arity: " + arguments.length;
  };
  partial.cljs$lang$maxFixedArity = 4;
  partial.cljs$lang$applyTo = partial__5.cljs$lang$applyTo;
  partial.cljs$lang$arity$2 = partial__2;
  partial.cljs$lang$arity$3 = partial__3;
  partial.cljs$lang$arity$4 = partial__4;
  partial.cljs$lang$arity$variadic = partial__5.cljs$lang$arity$variadic;
  return partial
}();
cljs.core.fnil = function() {
  var fnil = null;
  var fnil__2 = function(f, x) {
    return function() {
      var G__17787 = null;
      var G__17787__1 = function(a) {
        return f.call(null, a == null ? x : a)
      };
      var G__17787__2 = function(a, b) {
        return f.call(null, a == null ? x : a, b)
      };
      var G__17787__3 = function(a, b, c) {
        return f.call(null, a == null ? x : a, b, c)
      };
      var G__17787__4 = function() {
        var G__17788__delegate = function(a, b, c, ds) {
          return cljs.core.apply.call(null, f, a == null ? x : a, b, c, ds)
        };
        var G__17788 = function(a, b, c, var_args) {
          var ds = null;
          if(goog.isDef(var_args)) {
            ds = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__17788__delegate.call(this, a, b, c, ds)
        };
        G__17788.cljs$lang$maxFixedArity = 3;
        G__17788.cljs$lang$applyTo = function(arglist__17789) {
          var a = cljs.core.first(arglist__17789);
          var b = cljs.core.first(cljs.core.next(arglist__17789));
          var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17789)));
          var ds = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17789)));
          return G__17788__delegate(a, b, c, ds)
        };
        G__17788.cljs$lang$arity$variadic = G__17788__delegate;
        return G__17788
      }();
      G__17787 = function(a, b, c, var_args) {
        var ds = var_args;
        switch(arguments.length) {
          case 1:
            return G__17787__1.call(this, a);
          case 2:
            return G__17787__2.call(this, a, b);
          case 3:
            return G__17787__3.call(this, a, b, c);
          default:
            return G__17787__4.cljs$lang$arity$variadic(a, b, c, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__17787.cljs$lang$maxFixedArity = 3;
      G__17787.cljs$lang$applyTo = G__17787__4.cljs$lang$applyTo;
      return G__17787
    }()
  };
  var fnil__3 = function(f, x, y) {
    return function() {
      var G__17790 = null;
      var G__17790__2 = function(a, b) {
        return f.call(null, a == null ? x : a, b == null ? y : b)
      };
      var G__17790__3 = function(a, b, c) {
        return f.call(null, a == null ? x : a, b == null ? y : b, c)
      };
      var G__17790__4 = function() {
        var G__17791__delegate = function(a, b, c, ds) {
          return cljs.core.apply.call(null, f, a == null ? x : a, b == null ? y : b, c, ds)
        };
        var G__17791 = function(a, b, c, var_args) {
          var ds = null;
          if(goog.isDef(var_args)) {
            ds = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__17791__delegate.call(this, a, b, c, ds)
        };
        G__17791.cljs$lang$maxFixedArity = 3;
        G__17791.cljs$lang$applyTo = function(arglist__17792) {
          var a = cljs.core.first(arglist__17792);
          var b = cljs.core.first(cljs.core.next(arglist__17792));
          var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17792)));
          var ds = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17792)));
          return G__17791__delegate(a, b, c, ds)
        };
        G__17791.cljs$lang$arity$variadic = G__17791__delegate;
        return G__17791
      }();
      G__17790 = function(a, b, c, var_args) {
        var ds = var_args;
        switch(arguments.length) {
          case 2:
            return G__17790__2.call(this, a, b);
          case 3:
            return G__17790__3.call(this, a, b, c);
          default:
            return G__17790__4.cljs$lang$arity$variadic(a, b, c, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__17790.cljs$lang$maxFixedArity = 3;
      G__17790.cljs$lang$applyTo = G__17790__4.cljs$lang$applyTo;
      return G__17790
    }()
  };
  var fnil__4 = function(f, x, y, z) {
    return function() {
      var G__17793 = null;
      var G__17793__2 = function(a, b) {
        return f.call(null, a == null ? x : a, b == null ? y : b)
      };
      var G__17793__3 = function(a, b, c) {
        return f.call(null, a == null ? x : a, b == null ? y : b, c == null ? z : c)
      };
      var G__17793__4 = function() {
        var G__17794__delegate = function(a, b, c, ds) {
          return cljs.core.apply.call(null, f, a == null ? x : a, b == null ? y : b, c == null ? z : c, ds)
        };
        var G__17794 = function(a, b, c, var_args) {
          var ds = null;
          if(goog.isDef(var_args)) {
            ds = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__17794__delegate.call(this, a, b, c, ds)
        };
        G__17794.cljs$lang$maxFixedArity = 3;
        G__17794.cljs$lang$applyTo = function(arglist__17795) {
          var a = cljs.core.first(arglist__17795);
          var b = cljs.core.first(cljs.core.next(arglist__17795));
          var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17795)));
          var ds = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17795)));
          return G__17794__delegate(a, b, c, ds)
        };
        G__17794.cljs$lang$arity$variadic = G__17794__delegate;
        return G__17794
      }();
      G__17793 = function(a, b, c, var_args) {
        var ds = var_args;
        switch(arguments.length) {
          case 2:
            return G__17793__2.call(this, a, b);
          case 3:
            return G__17793__3.call(this, a, b, c);
          default:
            return G__17793__4.cljs$lang$arity$variadic(a, b, c, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__17793.cljs$lang$maxFixedArity = 3;
      G__17793.cljs$lang$applyTo = G__17793__4.cljs$lang$applyTo;
      return G__17793
    }()
  };
  fnil = function(f, x, y, z) {
    switch(arguments.length) {
      case 2:
        return fnil__2.call(this, f, x);
      case 3:
        return fnil__3.call(this, f, x, y);
      case 4:
        return fnil__4.call(this, f, x, y, z)
    }
    throw"Invalid arity: " + arguments.length;
  };
  fnil.cljs$lang$arity$2 = fnil__2;
  fnil.cljs$lang$arity$3 = fnil__3;
  fnil.cljs$lang$arity$4 = fnil__4;
  return fnil
}();
cljs.core.map_indexed = function map_indexed(f, coll) {
  var mapi__17811 = function mapi(idx, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____17819 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____17819) {
        var s__17820 = temp__3974__auto____17819;
        if(cljs.core.chunked_seq_QMARK_.call(null, s__17820)) {
          var c__17821 = cljs.core.chunk_first.call(null, s__17820);
          var size__17822 = cljs.core.count.call(null, c__17821);
          var b__17823 = cljs.core.chunk_buffer.call(null, size__17822);
          var n__2593__auto____17824 = size__17822;
          var i__17825 = 0;
          while(true) {
            if(i__17825 < n__2593__auto____17824) {
              cljs.core.chunk_append.call(null, b__17823, f.call(null, idx + i__17825, cljs.core._nth.call(null, c__17821, i__17825)));
              var G__17826 = i__17825 + 1;
              i__17825 = G__17826;
              continue
            }else {
            }
            break
          }
          return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__17823), mapi.call(null, idx + size__17822, cljs.core.chunk_rest.call(null, s__17820)))
        }else {
          return cljs.core.cons.call(null, f.call(null, idx, cljs.core.first.call(null, s__17820)), mapi.call(null, idx + 1, cljs.core.rest.call(null, s__17820)))
        }
      }else {
        return null
      }
    }, null)
  };
  return mapi__17811.call(null, 0, coll)
};
cljs.core.keep = function keep(f, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____17836 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____17836) {
      var s__17837 = temp__3974__auto____17836;
      if(cljs.core.chunked_seq_QMARK_.call(null, s__17837)) {
        var c__17838 = cljs.core.chunk_first.call(null, s__17837);
        var size__17839 = cljs.core.count.call(null, c__17838);
        var b__17840 = cljs.core.chunk_buffer.call(null, size__17839);
        var n__2593__auto____17841 = size__17839;
        var i__17842 = 0;
        while(true) {
          if(i__17842 < n__2593__auto____17841) {
            var x__17843 = f.call(null, cljs.core._nth.call(null, c__17838, i__17842));
            if(x__17843 == null) {
            }else {
              cljs.core.chunk_append.call(null, b__17840, x__17843)
            }
            var G__17845 = i__17842 + 1;
            i__17842 = G__17845;
            continue
          }else {
          }
          break
        }
        return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__17840), keep.call(null, f, cljs.core.chunk_rest.call(null, s__17837)))
      }else {
        var x__17844 = f.call(null, cljs.core.first.call(null, s__17837));
        if(x__17844 == null) {
          return keep.call(null, f, cljs.core.rest.call(null, s__17837))
        }else {
          return cljs.core.cons.call(null, x__17844, keep.call(null, f, cljs.core.rest.call(null, s__17837)))
        }
      }
    }else {
      return null
    }
  }, null)
};
cljs.core.keep_indexed = function keep_indexed(f, coll) {
  var keepi__17871 = function keepi(idx, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____17881 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____17881) {
        var s__17882 = temp__3974__auto____17881;
        if(cljs.core.chunked_seq_QMARK_.call(null, s__17882)) {
          var c__17883 = cljs.core.chunk_first.call(null, s__17882);
          var size__17884 = cljs.core.count.call(null, c__17883);
          var b__17885 = cljs.core.chunk_buffer.call(null, size__17884);
          var n__2593__auto____17886 = size__17884;
          var i__17887 = 0;
          while(true) {
            if(i__17887 < n__2593__auto____17886) {
              var x__17888 = f.call(null, idx + i__17887, cljs.core._nth.call(null, c__17883, i__17887));
              if(x__17888 == null) {
              }else {
                cljs.core.chunk_append.call(null, b__17885, x__17888)
              }
              var G__17890 = i__17887 + 1;
              i__17887 = G__17890;
              continue
            }else {
            }
            break
          }
          return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__17885), keepi.call(null, idx + size__17884, cljs.core.chunk_rest.call(null, s__17882)))
        }else {
          var x__17889 = f.call(null, idx, cljs.core.first.call(null, s__17882));
          if(x__17889 == null) {
            return keepi.call(null, idx + 1, cljs.core.rest.call(null, s__17882))
          }else {
            return cljs.core.cons.call(null, x__17889, keepi.call(null, idx + 1, cljs.core.rest.call(null, s__17882)))
          }
        }
      }else {
        return null
      }
    }, null)
  };
  return keepi__17871.call(null, 0, coll)
};
cljs.core.every_pred = function() {
  var every_pred = null;
  var every_pred__1 = function(p) {
    return function() {
      var ep1 = null;
      var ep1__0 = function() {
        return true
      };
      var ep1__1 = function(x) {
        return cljs.core.boolean$.call(null, p.call(null, x))
      };
      var ep1__2 = function(x, y) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____17976 = p.call(null, x);
          if(cljs.core.truth_(and__3822__auto____17976)) {
            return p.call(null, y)
          }else {
            return and__3822__auto____17976
          }
        }())
      };
      var ep1__3 = function(x, y, z) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____17977 = p.call(null, x);
          if(cljs.core.truth_(and__3822__auto____17977)) {
            var and__3822__auto____17978 = p.call(null, y);
            if(cljs.core.truth_(and__3822__auto____17978)) {
              return p.call(null, z)
            }else {
              return and__3822__auto____17978
            }
          }else {
            return and__3822__auto____17977
          }
        }())
      };
      var ep1__4 = function() {
        var G__18047__delegate = function(x, y, z, args) {
          return cljs.core.boolean$.call(null, function() {
            var and__3822__auto____17979 = ep1.call(null, x, y, z);
            if(cljs.core.truth_(and__3822__auto____17979)) {
              return cljs.core.every_QMARK_.call(null, p, args)
            }else {
              return and__3822__auto____17979
            }
          }())
        };
        var G__18047 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18047__delegate.call(this, x, y, z, args)
        };
        G__18047.cljs$lang$maxFixedArity = 3;
        G__18047.cljs$lang$applyTo = function(arglist__18048) {
          var x = cljs.core.first(arglist__18048);
          var y = cljs.core.first(cljs.core.next(arglist__18048));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18048)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18048)));
          return G__18047__delegate(x, y, z, args)
        };
        G__18047.cljs$lang$arity$variadic = G__18047__delegate;
        return G__18047
      }();
      ep1 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return ep1__0.call(this);
          case 1:
            return ep1__1.call(this, x);
          case 2:
            return ep1__2.call(this, x, y);
          case 3:
            return ep1__3.call(this, x, y, z);
          default:
            return ep1__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      ep1.cljs$lang$maxFixedArity = 3;
      ep1.cljs$lang$applyTo = ep1__4.cljs$lang$applyTo;
      ep1.cljs$lang$arity$0 = ep1__0;
      ep1.cljs$lang$arity$1 = ep1__1;
      ep1.cljs$lang$arity$2 = ep1__2;
      ep1.cljs$lang$arity$3 = ep1__3;
      ep1.cljs$lang$arity$variadic = ep1__4.cljs$lang$arity$variadic;
      return ep1
    }()
  };
  var every_pred__2 = function(p1, p2) {
    return function() {
      var ep2 = null;
      var ep2__0 = function() {
        return true
      };
      var ep2__1 = function(x) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____17991 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____17991)) {
            return p2.call(null, x)
          }else {
            return and__3822__auto____17991
          }
        }())
      };
      var ep2__2 = function(x, y) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____17992 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____17992)) {
            var and__3822__auto____17993 = p1.call(null, y);
            if(cljs.core.truth_(and__3822__auto____17993)) {
              var and__3822__auto____17994 = p2.call(null, x);
              if(cljs.core.truth_(and__3822__auto____17994)) {
                return p2.call(null, y)
              }else {
                return and__3822__auto____17994
              }
            }else {
              return and__3822__auto____17993
            }
          }else {
            return and__3822__auto____17992
          }
        }())
      };
      var ep2__3 = function(x, y, z) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____17995 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____17995)) {
            var and__3822__auto____17996 = p1.call(null, y);
            if(cljs.core.truth_(and__3822__auto____17996)) {
              var and__3822__auto____17997 = p1.call(null, z);
              if(cljs.core.truth_(and__3822__auto____17997)) {
                var and__3822__auto____17998 = p2.call(null, x);
                if(cljs.core.truth_(and__3822__auto____17998)) {
                  var and__3822__auto____17999 = p2.call(null, y);
                  if(cljs.core.truth_(and__3822__auto____17999)) {
                    return p2.call(null, z)
                  }else {
                    return and__3822__auto____17999
                  }
                }else {
                  return and__3822__auto____17998
                }
              }else {
                return and__3822__auto____17997
              }
            }else {
              return and__3822__auto____17996
            }
          }else {
            return and__3822__auto____17995
          }
        }())
      };
      var ep2__4 = function() {
        var G__18049__delegate = function(x, y, z, args) {
          return cljs.core.boolean$.call(null, function() {
            var and__3822__auto____18000 = ep2.call(null, x, y, z);
            if(cljs.core.truth_(and__3822__auto____18000)) {
              return cljs.core.every_QMARK_.call(null, function(p1__17846_SHARP_) {
                var and__3822__auto____18001 = p1.call(null, p1__17846_SHARP_);
                if(cljs.core.truth_(and__3822__auto____18001)) {
                  return p2.call(null, p1__17846_SHARP_)
                }else {
                  return and__3822__auto____18001
                }
              }, args)
            }else {
              return and__3822__auto____18000
            }
          }())
        };
        var G__18049 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18049__delegate.call(this, x, y, z, args)
        };
        G__18049.cljs$lang$maxFixedArity = 3;
        G__18049.cljs$lang$applyTo = function(arglist__18050) {
          var x = cljs.core.first(arglist__18050);
          var y = cljs.core.first(cljs.core.next(arglist__18050));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18050)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18050)));
          return G__18049__delegate(x, y, z, args)
        };
        G__18049.cljs$lang$arity$variadic = G__18049__delegate;
        return G__18049
      }();
      ep2 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return ep2__0.call(this);
          case 1:
            return ep2__1.call(this, x);
          case 2:
            return ep2__2.call(this, x, y);
          case 3:
            return ep2__3.call(this, x, y, z);
          default:
            return ep2__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      ep2.cljs$lang$maxFixedArity = 3;
      ep2.cljs$lang$applyTo = ep2__4.cljs$lang$applyTo;
      ep2.cljs$lang$arity$0 = ep2__0;
      ep2.cljs$lang$arity$1 = ep2__1;
      ep2.cljs$lang$arity$2 = ep2__2;
      ep2.cljs$lang$arity$3 = ep2__3;
      ep2.cljs$lang$arity$variadic = ep2__4.cljs$lang$arity$variadic;
      return ep2
    }()
  };
  var every_pred__3 = function(p1, p2, p3) {
    return function() {
      var ep3 = null;
      var ep3__0 = function() {
        return true
      };
      var ep3__1 = function(x) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18020 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18020)) {
            var and__3822__auto____18021 = p2.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18021)) {
              return p3.call(null, x)
            }else {
              return and__3822__auto____18021
            }
          }else {
            return and__3822__auto____18020
          }
        }())
      };
      var ep3__2 = function(x, y) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18022 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18022)) {
            var and__3822__auto____18023 = p2.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18023)) {
              var and__3822__auto____18024 = p3.call(null, x);
              if(cljs.core.truth_(and__3822__auto____18024)) {
                var and__3822__auto____18025 = p1.call(null, y);
                if(cljs.core.truth_(and__3822__auto____18025)) {
                  var and__3822__auto____18026 = p2.call(null, y);
                  if(cljs.core.truth_(and__3822__auto____18026)) {
                    return p3.call(null, y)
                  }else {
                    return and__3822__auto____18026
                  }
                }else {
                  return and__3822__auto____18025
                }
              }else {
                return and__3822__auto____18024
              }
            }else {
              return and__3822__auto____18023
            }
          }else {
            return and__3822__auto____18022
          }
        }())
      };
      var ep3__3 = function(x, y, z) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18027 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18027)) {
            var and__3822__auto____18028 = p2.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18028)) {
              var and__3822__auto____18029 = p3.call(null, x);
              if(cljs.core.truth_(and__3822__auto____18029)) {
                var and__3822__auto____18030 = p1.call(null, y);
                if(cljs.core.truth_(and__3822__auto____18030)) {
                  var and__3822__auto____18031 = p2.call(null, y);
                  if(cljs.core.truth_(and__3822__auto____18031)) {
                    var and__3822__auto____18032 = p3.call(null, y);
                    if(cljs.core.truth_(and__3822__auto____18032)) {
                      var and__3822__auto____18033 = p1.call(null, z);
                      if(cljs.core.truth_(and__3822__auto____18033)) {
                        var and__3822__auto____18034 = p2.call(null, z);
                        if(cljs.core.truth_(and__3822__auto____18034)) {
                          return p3.call(null, z)
                        }else {
                          return and__3822__auto____18034
                        }
                      }else {
                        return and__3822__auto____18033
                      }
                    }else {
                      return and__3822__auto____18032
                    }
                  }else {
                    return and__3822__auto____18031
                  }
                }else {
                  return and__3822__auto____18030
                }
              }else {
                return and__3822__auto____18029
              }
            }else {
              return and__3822__auto____18028
            }
          }else {
            return and__3822__auto____18027
          }
        }())
      };
      var ep3__4 = function() {
        var G__18051__delegate = function(x, y, z, args) {
          return cljs.core.boolean$.call(null, function() {
            var and__3822__auto____18035 = ep3.call(null, x, y, z);
            if(cljs.core.truth_(and__3822__auto____18035)) {
              return cljs.core.every_QMARK_.call(null, function(p1__17847_SHARP_) {
                var and__3822__auto____18036 = p1.call(null, p1__17847_SHARP_);
                if(cljs.core.truth_(and__3822__auto____18036)) {
                  var and__3822__auto____18037 = p2.call(null, p1__17847_SHARP_);
                  if(cljs.core.truth_(and__3822__auto____18037)) {
                    return p3.call(null, p1__17847_SHARP_)
                  }else {
                    return and__3822__auto____18037
                  }
                }else {
                  return and__3822__auto____18036
                }
              }, args)
            }else {
              return and__3822__auto____18035
            }
          }())
        };
        var G__18051 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18051__delegate.call(this, x, y, z, args)
        };
        G__18051.cljs$lang$maxFixedArity = 3;
        G__18051.cljs$lang$applyTo = function(arglist__18052) {
          var x = cljs.core.first(arglist__18052);
          var y = cljs.core.first(cljs.core.next(arglist__18052));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18052)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18052)));
          return G__18051__delegate(x, y, z, args)
        };
        G__18051.cljs$lang$arity$variadic = G__18051__delegate;
        return G__18051
      }();
      ep3 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return ep3__0.call(this);
          case 1:
            return ep3__1.call(this, x);
          case 2:
            return ep3__2.call(this, x, y);
          case 3:
            return ep3__3.call(this, x, y, z);
          default:
            return ep3__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      ep3.cljs$lang$maxFixedArity = 3;
      ep3.cljs$lang$applyTo = ep3__4.cljs$lang$applyTo;
      ep3.cljs$lang$arity$0 = ep3__0;
      ep3.cljs$lang$arity$1 = ep3__1;
      ep3.cljs$lang$arity$2 = ep3__2;
      ep3.cljs$lang$arity$3 = ep3__3;
      ep3.cljs$lang$arity$variadic = ep3__4.cljs$lang$arity$variadic;
      return ep3
    }()
  };
  var every_pred__4 = function() {
    var G__18053__delegate = function(p1, p2, p3, ps) {
      var ps__18038 = cljs.core.list_STAR_.call(null, p1, p2, p3, ps);
      return function() {
        var epn = null;
        var epn__0 = function() {
          return true
        };
        var epn__1 = function(x) {
          return cljs.core.every_QMARK_.call(null, function(p1__17848_SHARP_) {
            return p1__17848_SHARP_.call(null, x)
          }, ps__18038)
        };
        var epn__2 = function(x, y) {
          return cljs.core.every_QMARK_.call(null, function(p1__17849_SHARP_) {
            var and__3822__auto____18043 = p1__17849_SHARP_.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18043)) {
              return p1__17849_SHARP_.call(null, y)
            }else {
              return and__3822__auto____18043
            }
          }, ps__18038)
        };
        var epn__3 = function(x, y, z) {
          return cljs.core.every_QMARK_.call(null, function(p1__17850_SHARP_) {
            var and__3822__auto____18044 = p1__17850_SHARP_.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18044)) {
              var and__3822__auto____18045 = p1__17850_SHARP_.call(null, y);
              if(cljs.core.truth_(and__3822__auto____18045)) {
                return p1__17850_SHARP_.call(null, z)
              }else {
                return and__3822__auto____18045
              }
            }else {
              return and__3822__auto____18044
            }
          }, ps__18038)
        };
        var epn__4 = function() {
          var G__18054__delegate = function(x, y, z, args) {
            return cljs.core.boolean$.call(null, function() {
              var and__3822__auto____18046 = epn.call(null, x, y, z);
              if(cljs.core.truth_(and__3822__auto____18046)) {
                return cljs.core.every_QMARK_.call(null, function(p1__17851_SHARP_) {
                  return cljs.core.every_QMARK_.call(null, p1__17851_SHARP_, args)
                }, ps__18038)
              }else {
                return and__3822__auto____18046
              }
            }())
          };
          var G__18054 = function(x, y, z, var_args) {
            var args = null;
            if(goog.isDef(var_args)) {
              args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
            }
            return G__18054__delegate.call(this, x, y, z, args)
          };
          G__18054.cljs$lang$maxFixedArity = 3;
          G__18054.cljs$lang$applyTo = function(arglist__18055) {
            var x = cljs.core.first(arglist__18055);
            var y = cljs.core.first(cljs.core.next(arglist__18055));
            var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18055)));
            var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18055)));
            return G__18054__delegate(x, y, z, args)
          };
          G__18054.cljs$lang$arity$variadic = G__18054__delegate;
          return G__18054
        }();
        epn = function(x, y, z, var_args) {
          var args = var_args;
          switch(arguments.length) {
            case 0:
              return epn__0.call(this);
            case 1:
              return epn__1.call(this, x);
            case 2:
              return epn__2.call(this, x, y);
            case 3:
              return epn__3.call(this, x, y, z);
            default:
              return epn__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
          }
          throw"Invalid arity: " + arguments.length;
        };
        epn.cljs$lang$maxFixedArity = 3;
        epn.cljs$lang$applyTo = epn__4.cljs$lang$applyTo;
        epn.cljs$lang$arity$0 = epn__0;
        epn.cljs$lang$arity$1 = epn__1;
        epn.cljs$lang$arity$2 = epn__2;
        epn.cljs$lang$arity$3 = epn__3;
        epn.cljs$lang$arity$variadic = epn__4.cljs$lang$arity$variadic;
        return epn
      }()
    };
    var G__18053 = function(p1, p2, p3, var_args) {
      var ps = null;
      if(goog.isDef(var_args)) {
        ps = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__18053__delegate.call(this, p1, p2, p3, ps)
    };
    G__18053.cljs$lang$maxFixedArity = 3;
    G__18053.cljs$lang$applyTo = function(arglist__18056) {
      var p1 = cljs.core.first(arglist__18056);
      var p2 = cljs.core.first(cljs.core.next(arglist__18056));
      var p3 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18056)));
      var ps = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18056)));
      return G__18053__delegate(p1, p2, p3, ps)
    };
    G__18053.cljs$lang$arity$variadic = G__18053__delegate;
    return G__18053
  }();
  every_pred = function(p1, p2, p3, var_args) {
    var ps = var_args;
    switch(arguments.length) {
      case 1:
        return every_pred__1.call(this, p1);
      case 2:
        return every_pred__2.call(this, p1, p2);
      case 3:
        return every_pred__3.call(this, p1, p2, p3);
      default:
        return every_pred__4.cljs$lang$arity$variadic(p1, p2, p3, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  every_pred.cljs$lang$maxFixedArity = 3;
  every_pred.cljs$lang$applyTo = every_pred__4.cljs$lang$applyTo;
  every_pred.cljs$lang$arity$1 = every_pred__1;
  every_pred.cljs$lang$arity$2 = every_pred__2;
  every_pred.cljs$lang$arity$3 = every_pred__3;
  every_pred.cljs$lang$arity$variadic = every_pred__4.cljs$lang$arity$variadic;
  return every_pred
}();
cljs.core.some_fn = function() {
  var some_fn = null;
  var some_fn__1 = function(p) {
    return function() {
      var sp1 = null;
      var sp1__0 = function() {
        return null
      };
      var sp1__1 = function(x) {
        return p.call(null, x)
      };
      var sp1__2 = function(x, y) {
        var or__3824__auto____18137 = p.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18137)) {
          return or__3824__auto____18137
        }else {
          return p.call(null, y)
        }
      };
      var sp1__3 = function(x, y, z) {
        var or__3824__auto____18138 = p.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18138)) {
          return or__3824__auto____18138
        }else {
          var or__3824__auto____18139 = p.call(null, y);
          if(cljs.core.truth_(or__3824__auto____18139)) {
            return or__3824__auto____18139
          }else {
            return p.call(null, z)
          }
        }
      };
      var sp1__4 = function() {
        var G__18208__delegate = function(x, y, z, args) {
          var or__3824__auto____18140 = sp1.call(null, x, y, z);
          if(cljs.core.truth_(or__3824__auto____18140)) {
            return or__3824__auto____18140
          }else {
            return cljs.core.some.call(null, p, args)
          }
        };
        var G__18208 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18208__delegate.call(this, x, y, z, args)
        };
        G__18208.cljs$lang$maxFixedArity = 3;
        G__18208.cljs$lang$applyTo = function(arglist__18209) {
          var x = cljs.core.first(arglist__18209);
          var y = cljs.core.first(cljs.core.next(arglist__18209));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18209)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18209)));
          return G__18208__delegate(x, y, z, args)
        };
        G__18208.cljs$lang$arity$variadic = G__18208__delegate;
        return G__18208
      }();
      sp1 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return sp1__0.call(this);
          case 1:
            return sp1__1.call(this, x);
          case 2:
            return sp1__2.call(this, x, y);
          case 3:
            return sp1__3.call(this, x, y, z);
          default:
            return sp1__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      sp1.cljs$lang$maxFixedArity = 3;
      sp1.cljs$lang$applyTo = sp1__4.cljs$lang$applyTo;
      sp1.cljs$lang$arity$0 = sp1__0;
      sp1.cljs$lang$arity$1 = sp1__1;
      sp1.cljs$lang$arity$2 = sp1__2;
      sp1.cljs$lang$arity$3 = sp1__3;
      sp1.cljs$lang$arity$variadic = sp1__4.cljs$lang$arity$variadic;
      return sp1
    }()
  };
  var some_fn__2 = function(p1, p2) {
    return function() {
      var sp2 = null;
      var sp2__0 = function() {
        return null
      };
      var sp2__1 = function(x) {
        var or__3824__auto____18152 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18152)) {
          return or__3824__auto____18152
        }else {
          return p2.call(null, x)
        }
      };
      var sp2__2 = function(x, y) {
        var or__3824__auto____18153 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18153)) {
          return or__3824__auto____18153
        }else {
          var or__3824__auto____18154 = p1.call(null, y);
          if(cljs.core.truth_(or__3824__auto____18154)) {
            return or__3824__auto____18154
          }else {
            var or__3824__auto____18155 = p2.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18155)) {
              return or__3824__auto____18155
            }else {
              return p2.call(null, y)
            }
          }
        }
      };
      var sp2__3 = function(x, y, z) {
        var or__3824__auto____18156 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18156)) {
          return or__3824__auto____18156
        }else {
          var or__3824__auto____18157 = p1.call(null, y);
          if(cljs.core.truth_(or__3824__auto____18157)) {
            return or__3824__auto____18157
          }else {
            var or__3824__auto____18158 = p1.call(null, z);
            if(cljs.core.truth_(or__3824__auto____18158)) {
              return or__3824__auto____18158
            }else {
              var or__3824__auto____18159 = p2.call(null, x);
              if(cljs.core.truth_(or__3824__auto____18159)) {
                return or__3824__auto____18159
              }else {
                var or__3824__auto____18160 = p2.call(null, y);
                if(cljs.core.truth_(or__3824__auto____18160)) {
                  return or__3824__auto____18160
                }else {
                  return p2.call(null, z)
                }
              }
            }
          }
        }
      };
      var sp2__4 = function() {
        var G__18210__delegate = function(x, y, z, args) {
          var or__3824__auto____18161 = sp2.call(null, x, y, z);
          if(cljs.core.truth_(or__3824__auto____18161)) {
            return or__3824__auto____18161
          }else {
            return cljs.core.some.call(null, function(p1__17891_SHARP_) {
              var or__3824__auto____18162 = p1.call(null, p1__17891_SHARP_);
              if(cljs.core.truth_(or__3824__auto____18162)) {
                return or__3824__auto____18162
              }else {
                return p2.call(null, p1__17891_SHARP_)
              }
            }, args)
          }
        };
        var G__18210 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18210__delegate.call(this, x, y, z, args)
        };
        G__18210.cljs$lang$maxFixedArity = 3;
        G__18210.cljs$lang$applyTo = function(arglist__18211) {
          var x = cljs.core.first(arglist__18211);
          var y = cljs.core.first(cljs.core.next(arglist__18211));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18211)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18211)));
          return G__18210__delegate(x, y, z, args)
        };
        G__18210.cljs$lang$arity$variadic = G__18210__delegate;
        return G__18210
      }();
      sp2 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return sp2__0.call(this);
          case 1:
            return sp2__1.call(this, x);
          case 2:
            return sp2__2.call(this, x, y);
          case 3:
            return sp2__3.call(this, x, y, z);
          default:
            return sp2__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      sp2.cljs$lang$maxFixedArity = 3;
      sp2.cljs$lang$applyTo = sp2__4.cljs$lang$applyTo;
      sp2.cljs$lang$arity$0 = sp2__0;
      sp2.cljs$lang$arity$1 = sp2__1;
      sp2.cljs$lang$arity$2 = sp2__2;
      sp2.cljs$lang$arity$3 = sp2__3;
      sp2.cljs$lang$arity$variadic = sp2__4.cljs$lang$arity$variadic;
      return sp2
    }()
  };
  var some_fn__3 = function(p1, p2, p3) {
    return function() {
      var sp3 = null;
      var sp3__0 = function() {
        return null
      };
      var sp3__1 = function(x) {
        var or__3824__auto____18181 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18181)) {
          return or__3824__auto____18181
        }else {
          var or__3824__auto____18182 = p2.call(null, x);
          if(cljs.core.truth_(or__3824__auto____18182)) {
            return or__3824__auto____18182
          }else {
            return p3.call(null, x)
          }
        }
      };
      var sp3__2 = function(x, y) {
        var or__3824__auto____18183 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18183)) {
          return or__3824__auto____18183
        }else {
          var or__3824__auto____18184 = p2.call(null, x);
          if(cljs.core.truth_(or__3824__auto____18184)) {
            return or__3824__auto____18184
          }else {
            var or__3824__auto____18185 = p3.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18185)) {
              return or__3824__auto____18185
            }else {
              var or__3824__auto____18186 = p1.call(null, y);
              if(cljs.core.truth_(or__3824__auto____18186)) {
                return or__3824__auto____18186
              }else {
                var or__3824__auto____18187 = p2.call(null, y);
                if(cljs.core.truth_(or__3824__auto____18187)) {
                  return or__3824__auto____18187
                }else {
                  return p3.call(null, y)
                }
              }
            }
          }
        }
      };
      var sp3__3 = function(x, y, z) {
        var or__3824__auto____18188 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18188)) {
          return or__3824__auto____18188
        }else {
          var or__3824__auto____18189 = p2.call(null, x);
          if(cljs.core.truth_(or__3824__auto____18189)) {
            return or__3824__auto____18189
          }else {
            var or__3824__auto____18190 = p3.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18190)) {
              return or__3824__auto____18190
            }else {
              var or__3824__auto____18191 = p1.call(null, y);
              if(cljs.core.truth_(or__3824__auto____18191)) {
                return or__3824__auto____18191
              }else {
                var or__3824__auto____18192 = p2.call(null, y);
                if(cljs.core.truth_(or__3824__auto____18192)) {
                  return or__3824__auto____18192
                }else {
                  var or__3824__auto____18193 = p3.call(null, y);
                  if(cljs.core.truth_(or__3824__auto____18193)) {
                    return or__3824__auto____18193
                  }else {
                    var or__3824__auto____18194 = p1.call(null, z);
                    if(cljs.core.truth_(or__3824__auto____18194)) {
                      return or__3824__auto____18194
                    }else {
                      var or__3824__auto____18195 = p2.call(null, z);
                      if(cljs.core.truth_(or__3824__auto____18195)) {
                        return or__3824__auto____18195
                      }else {
                        return p3.call(null, z)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      };
      var sp3__4 = function() {
        var G__18212__delegate = function(x, y, z, args) {
          var or__3824__auto____18196 = sp3.call(null, x, y, z);
          if(cljs.core.truth_(or__3824__auto____18196)) {
            return or__3824__auto____18196
          }else {
            return cljs.core.some.call(null, function(p1__17892_SHARP_) {
              var or__3824__auto____18197 = p1.call(null, p1__17892_SHARP_);
              if(cljs.core.truth_(or__3824__auto____18197)) {
                return or__3824__auto____18197
              }else {
                var or__3824__auto____18198 = p2.call(null, p1__17892_SHARP_);
                if(cljs.core.truth_(or__3824__auto____18198)) {
                  return or__3824__auto____18198
                }else {
                  return p3.call(null, p1__17892_SHARP_)
                }
              }
            }, args)
          }
        };
        var G__18212 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18212__delegate.call(this, x, y, z, args)
        };
        G__18212.cljs$lang$maxFixedArity = 3;
        G__18212.cljs$lang$applyTo = function(arglist__18213) {
          var x = cljs.core.first(arglist__18213);
          var y = cljs.core.first(cljs.core.next(arglist__18213));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18213)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18213)));
          return G__18212__delegate(x, y, z, args)
        };
        G__18212.cljs$lang$arity$variadic = G__18212__delegate;
        return G__18212
      }();
      sp3 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return sp3__0.call(this);
          case 1:
            return sp3__1.call(this, x);
          case 2:
            return sp3__2.call(this, x, y);
          case 3:
            return sp3__3.call(this, x, y, z);
          default:
            return sp3__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      sp3.cljs$lang$maxFixedArity = 3;
      sp3.cljs$lang$applyTo = sp3__4.cljs$lang$applyTo;
      sp3.cljs$lang$arity$0 = sp3__0;
      sp3.cljs$lang$arity$1 = sp3__1;
      sp3.cljs$lang$arity$2 = sp3__2;
      sp3.cljs$lang$arity$3 = sp3__3;
      sp3.cljs$lang$arity$variadic = sp3__4.cljs$lang$arity$variadic;
      return sp3
    }()
  };
  var some_fn__4 = function() {
    var G__18214__delegate = function(p1, p2, p3, ps) {
      var ps__18199 = cljs.core.list_STAR_.call(null, p1, p2, p3, ps);
      return function() {
        var spn = null;
        var spn__0 = function() {
          return null
        };
        var spn__1 = function(x) {
          return cljs.core.some.call(null, function(p1__17893_SHARP_) {
            return p1__17893_SHARP_.call(null, x)
          }, ps__18199)
        };
        var spn__2 = function(x, y) {
          return cljs.core.some.call(null, function(p1__17894_SHARP_) {
            var or__3824__auto____18204 = p1__17894_SHARP_.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18204)) {
              return or__3824__auto____18204
            }else {
              return p1__17894_SHARP_.call(null, y)
            }
          }, ps__18199)
        };
        var spn__3 = function(x, y, z) {
          return cljs.core.some.call(null, function(p1__17895_SHARP_) {
            var or__3824__auto____18205 = p1__17895_SHARP_.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18205)) {
              return or__3824__auto____18205
            }else {
              var or__3824__auto____18206 = p1__17895_SHARP_.call(null, y);
              if(cljs.core.truth_(or__3824__auto____18206)) {
                return or__3824__auto____18206
              }else {
                return p1__17895_SHARP_.call(null, z)
              }
            }
          }, ps__18199)
        };
        var spn__4 = function() {
          var G__18215__delegate = function(x, y, z, args) {
            var or__3824__auto____18207 = spn.call(null, x, y, z);
            if(cljs.core.truth_(or__3824__auto____18207)) {
              return or__3824__auto____18207
            }else {
              return cljs.core.some.call(null, function(p1__17896_SHARP_) {
                return cljs.core.some.call(null, p1__17896_SHARP_, args)
              }, ps__18199)
            }
          };
          var G__18215 = function(x, y, z, var_args) {
            var args = null;
            if(goog.isDef(var_args)) {
              args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
            }
            return G__18215__delegate.call(this, x, y, z, args)
          };
          G__18215.cljs$lang$maxFixedArity = 3;
          G__18215.cljs$lang$applyTo = function(arglist__18216) {
            var x = cljs.core.first(arglist__18216);
            var y = cljs.core.first(cljs.core.next(arglist__18216));
            var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18216)));
            var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18216)));
            return G__18215__delegate(x, y, z, args)
          };
          G__18215.cljs$lang$arity$variadic = G__18215__delegate;
          return G__18215
        }();
        spn = function(x, y, z, var_args) {
          var args = var_args;
          switch(arguments.length) {
            case 0:
              return spn__0.call(this);
            case 1:
              return spn__1.call(this, x);
            case 2:
              return spn__2.call(this, x, y);
            case 3:
              return spn__3.call(this, x, y, z);
            default:
              return spn__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
          }
          throw"Invalid arity: " + arguments.length;
        };
        spn.cljs$lang$maxFixedArity = 3;
        spn.cljs$lang$applyTo = spn__4.cljs$lang$applyTo;
        spn.cljs$lang$arity$0 = spn__0;
        spn.cljs$lang$arity$1 = spn__1;
        spn.cljs$lang$arity$2 = spn__2;
        spn.cljs$lang$arity$3 = spn__3;
        spn.cljs$lang$arity$variadic = spn__4.cljs$lang$arity$variadic;
        return spn
      }()
    };
    var G__18214 = function(p1, p2, p3, var_args) {
      var ps = null;
      if(goog.isDef(var_args)) {
        ps = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__18214__delegate.call(this, p1, p2, p3, ps)
    };
    G__18214.cljs$lang$maxFixedArity = 3;
    G__18214.cljs$lang$applyTo = function(arglist__18217) {
      var p1 = cljs.core.first(arglist__18217);
      var p2 = cljs.core.first(cljs.core.next(arglist__18217));
      var p3 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18217)));
      var ps = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18217)));
      return G__18214__delegate(p1, p2, p3, ps)
    };
    G__18214.cljs$lang$arity$variadic = G__18214__delegate;
    return G__18214
  }();
  some_fn = function(p1, p2, p3, var_args) {
    var ps = var_args;
    switch(arguments.length) {
      case 1:
        return some_fn__1.call(this, p1);
      case 2:
        return some_fn__2.call(this, p1, p2);
      case 3:
        return some_fn__3.call(this, p1, p2, p3);
      default:
        return some_fn__4.cljs$lang$arity$variadic(p1, p2, p3, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  some_fn.cljs$lang$maxFixedArity = 3;
  some_fn.cljs$lang$applyTo = some_fn__4.cljs$lang$applyTo;
  some_fn.cljs$lang$arity$1 = some_fn__1;
  some_fn.cljs$lang$arity$2 = some_fn__2;
  some_fn.cljs$lang$arity$3 = some_fn__3;
  some_fn.cljs$lang$arity$variadic = some_fn__4.cljs$lang$arity$variadic;
  return some_fn
}();
cljs.core.map = function() {
  var map = null;
  var map__2 = function(f, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____18236 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18236) {
        var s__18237 = temp__3974__auto____18236;
        if(cljs.core.chunked_seq_QMARK_.call(null, s__18237)) {
          var c__18238 = cljs.core.chunk_first.call(null, s__18237);
          var size__18239 = cljs.core.count.call(null, c__18238);
          var b__18240 = cljs.core.chunk_buffer.call(null, size__18239);
          var n__2593__auto____18241 = size__18239;
          var i__18242 = 0;
          while(true) {
            if(i__18242 < n__2593__auto____18241) {
              cljs.core.chunk_append.call(null, b__18240, f.call(null, cljs.core._nth.call(null, c__18238, i__18242)));
              var G__18254 = i__18242 + 1;
              i__18242 = G__18254;
              continue
            }else {
            }
            break
          }
          return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18240), map.call(null, f, cljs.core.chunk_rest.call(null, s__18237)))
        }else {
          return cljs.core.cons.call(null, f.call(null, cljs.core.first.call(null, s__18237)), map.call(null, f, cljs.core.rest.call(null, s__18237)))
        }
      }else {
        return null
      }
    }, null)
  };
  var map__3 = function(f, c1, c2) {
    return new cljs.core.LazySeq(null, false, function() {
      var s1__18243 = cljs.core.seq.call(null, c1);
      var s2__18244 = cljs.core.seq.call(null, c2);
      if(function() {
        var and__3822__auto____18245 = s1__18243;
        if(and__3822__auto____18245) {
          return s2__18244
        }else {
          return and__3822__auto____18245
        }
      }()) {
        return cljs.core.cons.call(null, f.call(null, cljs.core.first.call(null, s1__18243), cljs.core.first.call(null, s2__18244)), map.call(null, f, cljs.core.rest.call(null, s1__18243), cljs.core.rest.call(null, s2__18244)))
      }else {
        return null
      }
    }, null)
  };
  var map__4 = function(f, c1, c2, c3) {
    return new cljs.core.LazySeq(null, false, function() {
      var s1__18246 = cljs.core.seq.call(null, c1);
      var s2__18247 = cljs.core.seq.call(null, c2);
      var s3__18248 = cljs.core.seq.call(null, c3);
      if(function() {
        var and__3822__auto____18249 = s1__18246;
        if(and__3822__auto____18249) {
          var and__3822__auto____18250 = s2__18247;
          if(and__3822__auto____18250) {
            return s3__18248
          }else {
            return and__3822__auto____18250
          }
        }else {
          return and__3822__auto____18249
        }
      }()) {
        return cljs.core.cons.call(null, f.call(null, cljs.core.first.call(null, s1__18246), cljs.core.first.call(null, s2__18247), cljs.core.first.call(null, s3__18248)), map.call(null, f, cljs.core.rest.call(null, s1__18246), cljs.core.rest.call(null, s2__18247), cljs.core.rest.call(null, s3__18248)))
      }else {
        return null
      }
    }, null)
  };
  var map__5 = function() {
    var G__18255__delegate = function(f, c1, c2, c3, colls) {
      var step__18253 = function step(cs) {
        return new cljs.core.LazySeq(null, false, function() {
          var ss__18252 = map.call(null, cljs.core.seq, cs);
          if(cljs.core.every_QMARK_.call(null, cljs.core.identity, ss__18252)) {
            return cljs.core.cons.call(null, map.call(null, cljs.core.first, ss__18252), step.call(null, map.call(null, cljs.core.rest, ss__18252)))
          }else {
            return null
          }
        }, null)
      };
      return map.call(null, function(p1__18057_SHARP_) {
        return cljs.core.apply.call(null, f, p1__18057_SHARP_)
      }, step__18253.call(null, cljs.core.conj.call(null, colls, c3, c2, c1)))
    };
    var G__18255 = function(f, c1, c2, c3, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__18255__delegate.call(this, f, c1, c2, c3, colls)
    };
    G__18255.cljs$lang$maxFixedArity = 4;
    G__18255.cljs$lang$applyTo = function(arglist__18256) {
      var f = cljs.core.first(arglist__18256);
      var c1 = cljs.core.first(cljs.core.next(arglist__18256));
      var c2 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18256)));
      var c3 = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18256))));
      var colls = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18256))));
      return G__18255__delegate(f, c1, c2, c3, colls)
    };
    G__18255.cljs$lang$arity$variadic = G__18255__delegate;
    return G__18255
  }();
  map = function(f, c1, c2, c3, var_args) {
    var colls = var_args;
    switch(arguments.length) {
      case 2:
        return map__2.call(this, f, c1);
      case 3:
        return map__3.call(this, f, c1, c2);
      case 4:
        return map__4.call(this, f, c1, c2, c3);
      default:
        return map__5.cljs$lang$arity$variadic(f, c1, c2, c3, cljs.core.array_seq(arguments, 4))
    }
    throw"Invalid arity: " + arguments.length;
  };
  map.cljs$lang$maxFixedArity = 4;
  map.cljs$lang$applyTo = map__5.cljs$lang$applyTo;
  map.cljs$lang$arity$2 = map__2;
  map.cljs$lang$arity$3 = map__3;
  map.cljs$lang$arity$4 = map__4;
  map.cljs$lang$arity$variadic = map__5.cljs$lang$arity$variadic;
  return map
}();
cljs.core.take = function take(n, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    if(n > 0) {
      var temp__3974__auto____18259 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18259) {
        var s__18260 = temp__3974__auto____18259;
        return cljs.core.cons.call(null, cljs.core.first.call(null, s__18260), take.call(null, n - 1, cljs.core.rest.call(null, s__18260)))
      }else {
        return null
      }
    }else {
      return null
    }
  }, null)
};
cljs.core.drop = function drop(n, coll) {
  var step__18266 = function(n, coll) {
    while(true) {
      var s__18264 = cljs.core.seq.call(null, coll);
      if(cljs.core.truth_(function() {
        var and__3822__auto____18265 = n > 0;
        if(and__3822__auto____18265) {
          return s__18264
        }else {
          return and__3822__auto____18265
        }
      }())) {
        var G__18267 = n - 1;
        var G__18268 = cljs.core.rest.call(null, s__18264);
        n = G__18267;
        coll = G__18268;
        continue
      }else {
        return s__18264
      }
      break
    }
  };
  return new cljs.core.LazySeq(null, false, function() {
    return step__18266.call(null, n, coll)
  }, null)
};
cljs.core.drop_last = function() {
  var drop_last = null;
  var drop_last__1 = function(s) {
    return drop_last.call(null, 1, s)
  };
  var drop_last__2 = function(n, s) {
    return cljs.core.map.call(null, function(x, _) {
      return x
    }, s, cljs.core.drop.call(null, n, s))
  };
  drop_last = function(n, s) {
    switch(arguments.length) {
      case 1:
        return drop_last__1.call(this, n);
      case 2:
        return drop_last__2.call(this, n, s)
    }
    throw"Invalid arity: " + arguments.length;
  };
  drop_last.cljs$lang$arity$1 = drop_last__1;
  drop_last.cljs$lang$arity$2 = drop_last__2;
  return drop_last
}();
cljs.core.take_last = function take_last(n, coll) {
  var s__18271 = cljs.core.seq.call(null, coll);
  var lead__18272 = cljs.core.seq.call(null, cljs.core.drop.call(null, n, coll));
  while(true) {
    if(lead__18272) {
      var G__18273 = cljs.core.next.call(null, s__18271);
      var G__18274 = cljs.core.next.call(null, lead__18272);
      s__18271 = G__18273;
      lead__18272 = G__18274;
      continue
    }else {
      return s__18271
    }
    break
  }
};
cljs.core.drop_while = function drop_while(pred, coll) {
  var step__18280 = function(pred, coll) {
    while(true) {
      var s__18278 = cljs.core.seq.call(null, coll);
      if(cljs.core.truth_(function() {
        var and__3822__auto____18279 = s__18278;
        if(and__3822__auto____18279) {
          return pred.call(null, cljs.core.first.call(null, s__18278))
        }else {
          return and__3822__auto____18279
        }
      }())) {
        var G__18281 = pred;
        var G__18282 = cljs.core.rest.call(null, s__18278);
        pred = G__18281;
        coll = G__18282;
        continue
      }else {
        return s__18278
      }
      break
    }
  };
  return new cljs.core.LazySeq(null, false, function() {
    return step__18280.call(null, pred, coll)
  }, null)
};
cljs.core.cycle = function cycle(coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____18285 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____18285) {
      var s__18286 = temp__3974__auto____18285;
      return cljs.core.concat.call(null, s__18286, cycle.call(null, s__18286))
    }else {
      return null
    }
  }, null)
};
cljs.core.split_at = function split_at(n, coll) {
  return cljs.core.PersistentVector.fromArray([cljs.core.take.call(null, n, coll), cljs.core.drop.call(null, n, coll)], true)
};
cljs.core.repeat = function() {
  var repeat = null;
  var repeat__1 = function(x) {
    return new cljs.core.LazySeq(null, false, function() {
      return cljs.core.cons.call(null, x, repeat.call(null, x))
    }, null)
  };
  var repeat__2 = function(n, x) {
    return cljs.core.take.call(null, n, repeat.call(null, x))
  };
  repeat = function(n, x) {
    switch(arguments.length) {
      case 1:
        return repeat__1.call(this, n);
      case 2:
        return repeat__2.call(this, n, x)
    }
    throw"Invalid arity: " + arguments.length;
  };
  repeat.cljs$lang$arity$1 = repeat__1;
  repeat.cljs$lang$arity$2 = repeat__2;
  return repeat
}();
cljs.core.replicate = function replicate(n, x) {
  return cljs.core.take.call(null, n, cljs.core.repeat.call(null, x))
};
cljs.core.repeatedly = function() {
  var repeatedly = null;
  var repeatedly__1 = function(f) {
    return new cljs.core.LazySeq(null, false, function() {
      return cljs.core.cons.call(null, f.call(null), repeatedly.call(null, f))
    }, null)
  };
  var repeatedly__2 = function(n, f) {
    return cljs.core.take.call(null, n, repeatedly.call(null, f))
  };
  repeatedly = function(n, f) {
    switch(arguments.length) {
      case 1:
        return repeatedly__1.call(this, n);
      case 2:
        return repeatedly__2.call(this, n, f)
    }
    throw"Invalid arity: " + arguments.length;
  };
  repeatedly.cljs$lang$arity$1 = repeatedly__1;
  repeatedly.cljs$lang$arity$2 = repeatedly__2;
  return repeatedly
}();
cljs.core.iterate = function iterate(f, x) {
  return cljs.core.cons.call(null, x, new cljs.core.LazySeq(null, false, function() {
    return iterate.call(null, f, f.call(null, x))
  }, null))
};
cljs.core.interleave = function() {
  var interleave = null;
  var interleave__2 = function(c1, c2) {
    return new cljs.core.LazySeq(null, false, function() {
      var s1__18291 = cljs.core.seq.call(null, c1);
      var s2__18292 = cljs.core.seq.call(null, c2);
      if(function() {
        var and__3822__auto____18293 = s1__18291;
        if(and__3822__auto____18293) {
          return s2__18292
        }else {
          return and__3822__auto____18293
        }
      }()) {
        return cljs.core.cons.call(null, cljs.core.first.call(null, s1__18291), cljs.core.cons.call(null, cljs.core.first.call(null, s2__18292), interleave.call(null, cljs.core.rest.call(null, s1__18291), cljs.core.rest.call(null, s2__18292))))
      }else {
        return null
      }
    }, null)
  };
  var interleave__3 = function() {
    var G__18295__delegate = function(c1, c2, colls) {
      return new cljs.core.LazySeq(null, false, function() {
        var ss__18294 = cljs.core.map.call(null, cljs.core.seq, cljs.core.conj.call(null, colls, c2, c1));
        if(cljs.core.every_QMARK_.call(null, cljs.core.identity, ss__18294)) {
          return cljs.core.concat.call(null, cljs.core.map.call(null, cljs.core.first, ss__18294), cljs.core.apply.call(null, interleave, cljs.core.map.call(null, cljs.core.rest, ss__18294)))
        }else {
          return null
        }
      }, null)
    };
    var G__18295 = function(c1, c2, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__18295__delegate.call(this, c1, c2, colls)
    };
    G__18295.cljs$lang$maxFixedArity = 2;
    G__18295.cljs$lang$applyTo = function(arglist__18296) {
      var c1 = cljs.core.first(arglist__18296);
      var c2 = cljs.core.first(cljs.core.next(arglist__18296));
      var colls = cljs.core.rest(cljs.core.next(arglist__18296));
      return G__18295__delegate(c1, c2, colls)
    };
    G__18295.cljs$lang$arity$variadic = G__18295__delegate;
    return G__18295
  }();
  interleave = function(c1, c2, var_args) {
    var colls = var_args;
    switch(arguments.length) {
      case 2:
        return interleave__2.call(this, c1, c2);
      default:
        return interleave__3.cljs$lang$arity$variadic(c1, c2, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  interleave.cljs$lang$maxFixedArity = 2;
  interleave.cljs$lang$applyTo = interleave__3.cljs$lang$applyTo;
  interleave.cljs$lang$arity$2 = interleave__2;
  interleave.cljs$lang$arity$variadic = interleave__3.cljs$lang$arity$variadic;
  return interleave
}();
cljs.core.interpose = function interpose(sep, coll) {
  return cljs.core.drop.call(null, 1, cljs.core.interleave.call(null, cljs.core.repeat.call(null, sep), coll))
};
cljs.core.flatten1 = function flatten1(colls) {
  var cat__18306 = function cat(coll, colls) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3971__auto____18304 = cljs.core.seq.call(null, coll);
      if(temp__3971__auto____18304) {
        var coll__18305 = temp__3971__auto____18304;
        return cljs.core.cons.call(null, cljs.core.first.call(null, coll__18305), cat.call(null, cljs.core.rest.call(null, coll__18305), colls))
      }else {
        if(cljs.core.seq.call(null, colls)) {
          return cat.call(null, cljs.core.first.call(null, colls), cljs.core.rest.call(null, colls))
        }else {
          return null
        }
      }
    }, null)
  };
  return cat__18306.call(null, null, colls)
};
cljs.core.mapcat = function() {
  var mapcat = null;
  var mapcat__2 = function(f, coll) {
    return cljs.core.flatten1.call(null, cljs.core.map.call(null, f, coll))
  };
  var mapcat__3 = function() {
    var G__18307__delegate = function(f, coll, colls) {
      return cljs.core.flatten1.call(null, cljs.core.apply.call(null, cljs.core.map, f, coll, colls))
    };
    var G__18307 = function(f, coll, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__18307__delegate.call(this, f, coll, colls)
    };
    G__18307.cljs$lang$maxFixedArity = 2;
    G__18307.cljs$lang$applyTo = function(arglist__18308) {
      var f = cljs.core.first(arglist__18308);
      var coll = cljs.core.first(cljs.core.next(arglist__18308));
      var colls = cljs.core.rest(cljs.core.next(arglist__18308));
      return G__18307__delegate(f, coll, colls)
    };
    G__18307.cljs$lang$arity$variadic = G__18307__delegate;
    return G__18307
  }();
  mapcat = function(f, coll, var_args) {
    var colls = var_args;
    switch(arguments.length) {
      case 2:
        return mapcat__2.call(this, f, coll);
      default:
        return mapcat__3.cljs$lang$arity$variadic(f, coll, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  mapcat.cljs$lang$maxFixedArity = 2;
  mapcat.cljs$lang$applyTo = mapcat__3.cljs$lang$applyTo;
  mapcat.cljs$lang$arity$2 = mapcat__2;
  mapcat.cljs$lang$arity$variadic = mapcat__3.cljs$lang$arity$variadic;
  return mapcat
}();
cljs.core.filter = function filter(pred, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____18318 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____18318) {
      var s__18319 = temp__3974__auto____18318;
      if(cljs.core.chunked_seq_QMARK_.call(null, s__18319)) {
        var c__18320 = cljs.core.chunk_first.call(null, s__18319);
        var size__18321 = cljs.core.count.call(null, c__18320);
        var b__18322 = cljs.core.chunk_buffer.call(null, size__18321);
        var n__2593__auto____18323 = size__18321;
        var i__18324 = 0;
        while(true) {
          if(i__18324 < n__2593__auto____18323) {
            if(cljs.core.truth_(pred.call(null, cljs.core._nth.call(null, c__18320, i__18324)))) {
              cljs.core.chunk_append.call(null, b__18322, cljs.core._nth.call(null, c__18320, i__18324))
            }else {
            }
            var G__18327 = i__18324 + 1;
            i__18324 = G__18327;
            continue
          }else {
          }
          break
        }
        return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18322), filter.call(null, pred, cljs.core.chunk_rest.call(null, s__18319)))
      }else {
        var f__18325 = cljs.core.first.call(null, s__18319);
        var r__18326 = cljs.core.rest.call(null, s__18319);
        if(cljs.core.truth_(pred.call(null, f__18325))) {
          return cljs.core.cons.call(null, f__18325, filter.call(null, pred, r__18326))
        }else {
          return filter.call(null, pred, r__18326)
        }
      }
    }else {
      return null
    }
  }, null)
};
cljs.core.remove = function remove(pred, coll) {
  return cljs.core.filter.call(null, cljs.core.complement.call(null, pred), coll)
};
cljs.core.tree_seq = function tree_seq(branch_QMARK_, children, root) {
  var walk__18330 = function walk(node) {
    return new cljs.core.LazySeq(null, false, function() {
      return cljs.core.cons.call(null, node, cljs.core.truth_(branch_QMARK_.call(null, node)) ? cljs.core.mapcat.call(null, walk, children.call(null, node)) : null)
    }, null)
  };
  return walk__18330.call(null, root)
};
cljs.core.flatten = function flatten(x) {
  return cljs.core.filter.call(null, function(p1__18328_SHARP_) {
    return!cljs.core.sequential_QMARK_.call(null, p1__18328_SHARP_)
  }, cljs.core.rest.call(null, cljs.core.tree_seq.call(null, cljs.core.sequential_QMARK_, cljs.core.seq, x)))
};
cljs.core.into = function into(to, from) {
  if(function() {
    var G__18334__18335 = to;
    if(G__18334__18335) {
      if(function() {
        var or__3824__auto____18336 = G__18334__18335.cljs$lang$protocol_mask$partition1$ & 4;
        if(or__3824__auto____18336) {
          return or__3824__auto____18336
        }else {
          return G__18334__18335.cljs$core$IEditableCollection$
        }
      }()) {
        return true
      }else {
        if(!G__18334__18335.cljs$lang$protocol_mask$partition1$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IEditableCollection, G__18334__18335)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IEditableCollection, G__18334__18335)
    }
  }()) {
    return cljs.core.persistent_BANG_.call(null, cljs.core.reduce.call(null, cljs.core._conj_BANG_, cljs.core.transient$.call(null, to), from))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, to, from)
  }
};
cljs.core.mapv = function() {
  var mapv = null;
  var mapv__2 = function(f, coll) {
    return cljs.core.persistent_BANG_.call(null, cljs.core.reduce.call(null, function(v, o) {
      return cljs.core.conj_BANG_.call(null, v, f.call(null, o))
    }, cljs.core.transient$.call(null, cljs.core.PersistentVector.EMPTY), coll))
  };
  var mapv__3 = function(f, c1, c2) {
    return cljs.core.into.call(null, cljs.core.PersistentVector.EMPTY, cljs.core.map.call(null, f, c1, c2))
  };
  var mapv__4 = function(f, c1, c2, c3) {
    return cljs.core.into.call(null, cljs.core.PersistentVector.EMPTY, cljs.core.map.call(null, f, c1, c2, c3))
  };
  var mapv__5 = function() {
    var G__18337__delegate = function(f, c1, c2, c3, colls) {
      return cljs.core.into.call(null, cljs.core.PersistentVector.EMPTY, cljs.core.apply.call(null, cljs.core.map, f, c1, c2, c3, colls))
    };
    var G__18337 = function(f, c1, c2, c3, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__18337__delegate.call(this, f, c1, c2, c3, colls)
    };
    G__18337.cljs$lang$maxFixedArity = 4;
    G__18337.cljs$lang$applyTo = function(arglist__18338) {
      var f = cljs.core.first(arglist__18338);
      var c1 = cljs.core.first(cljs.core.next(arglist__18338));
      var c2 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18338)));
      var c3 = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18338))));
      var colls = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18338))));
      return G__18337__delegate(f, c1, c2, c3, colls)
    };
    G__18337.cljs$lang$arity$variadic = G__18337__delegate;
    return G__18337
  }();
  mapv = function(f, c1, c2, c3, var_args) {
    var colls = var_args;
    switch(arguments.length) {
      case 2:
        return mapv__2.call(this, f, c1);
      case 3:
        return mapv__3.call(this, f, c1, c2);
      case 4:
        return mapv__4.call(this, f, c1, c2, c3);
      default:
        return mapv__5.cljs$lang$arity$variadic(f, c1, c2, c3, cljs.core.array_seq(arguments, 4))
    }
    throw"Invalid arity: " + arguments.length;
  };
  mapv.cljs$lang$maxFixedArity = 4;
  mapv.cljs$lang$applyTo = mapv__5.cljs$lang$applyTo;
  mapv.cljs$lang$arity$2 = mapv__2;
  mapv.cljs$lang$arity$3 = mapv__3;
  mapv.cljs$lang$arity$4 = mapv__4;
  mapv.cljs$lang$arity$variadic = mapv__5.cljs$lang$arity$variadic;
  return mapv
}();
cljs.core.filterv = function filterv(pred, coll) {
  return cljs.core.persistent_BANG_.call(null, cljs.core.reduce.call(null, function(v, o) {
    if(cljs.core.truth_(pred.call(null, o))) {
      return cljs.core.conj_BANG_.call(null, v, o)
    }else {
      return v
    }
  }, cljs.core.transient$.call(null, cljs.core.PersistentVector.EMPTY), coll))
};
cljs.core.partition = function() {
  var partition = null;
  var partition__2 = function(n, coll) {
    return partition.call(null, n, n, coll)
  };
  var partition__3 = function(n, step, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____18345 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18345) {
        var s__18346 = temp__3974__auto____18345;
        var p__18347 = cljs.core.take.call(null, n, s__18346);
        if(n === cljs.core.count.call(null, p__18347)) {
          return cljs.core.cons.call(null, p__18347, partition.call(null, n, step, cljs.core.drop.call(null, step, s__18346)))
        }else {
          return null
        }
      }else {
        return null
      }
    }, null)
  };
  var partition__4 = function(n, step, pad, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____18348 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18348) {
        var s__18349 = temp__3974__auto____18348;
        var p__18350 = cljs.core.take.call(null, n, s__18349);
        if(n === cljs.core.count.call(null, p__18350)) {
          return cljs.core.cons.call(null, p__18350, partition.call(null, n, step, pad, cljs.core.drop.call(null, step, s__18349)))
        }else {
          return cljs.core.list.call(null, cljs.core.take.call(null, n, cljs.core.concat.call(null, p__18350, pad)))
        }
      }else {
        return null
      }
    }, null)
  };
  partition = function(n, step, pad, coll) {
    switch(arguments.length) {
      case 2:
        return partition__2.call(this, n, step);
      case 3:
        return partition__3.call(this, n, step, pad);
      case 4:
        return partition__4.call(this, n, step, pad, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  partition.cljs$lang$arity$2 = partition__2;
  partition.cljs$lang$arity$3 = partition__3;
  partition.cljs$lang$arity$4 = partition__4;
  return partition
}();
cljs.core.get_in = function() {
  var get_in = null;
  var get_in__2 = function(m, ks) {
    return cljs.core.reduce.call(null, cljs.core.get, m, ks)
  };
  var get_in__3 = function(m, ks, not_found) {
    var sentinel__18355 = cljs.core.lookup_sentinel;
    var m__18356 = m;
    var ks__18357 = cljs.core.seq.call(null, ks);
    while(true) {
      if(ks__18357) {
        var m__18358 = cljs.core._lookup.call(null, m__18356, cljs.core.first.call(null, ks__18357), sentinel__18355);
        if(sentinel__18355 === m__18358) {
          return not_found
        }else {
          var G__18359 = sentinel__18355;
          var G__18360 = m__18358;
          var G__18361 = cljs.core.next.call(null, ks__18357);
          sentinel__18355 = G__18359;
          m__18356 = G__18360;
          ks__18357 = G__18361;
          continue
        }
      }else {
        return m__18356
      }
      break
    }
  };
  get_in = function(m, ks, not_found) {
    switch(arguments.length) {
      case 2:
        return get_in__2.call(this, m, ks);
      case 3:
        return get_in__3.call(this, m, ks, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  get_in.cljs$lang$arity$2 = get_in__2;
  get_in.cljs$lang$arity$3 = get_in__3;
  return get_in
}();
cljs.core.assoc_in = function assoc_in(m, p__18362, v) {
  var vec__18367__18368 = p__18362;
  var k__18369 = cljs.core.nth.call(null, vec__18367__18368, 0, null);
  var ks__18370 = cljs.core.nthnext.call(null, vec__18367__18368, 1);
  if(cljs.core.truth_(ks__18370)) {
    return cljs.core.assoc.call(null, m, k__18369, assoc_in.call(null, cljs.core._lookup.call(null, m, k__18369, null), ks__18370, v))
  }else {
    return cljs.core.assoc.call(null, m, k__18369, v)
  }
};
cljs.core.update_in = function() {
  var update_in__delegate = function(m, p__18371, f, args) {
    var vec__18376__18377 = p__18371;
    var k__18378 = cljs.core.nth.call(null, vec__18376__18377, 0, null);
    var ks__18379 = cljs.core.nthnext.call(null, vec__18376__18377, 1);
    if(cljs.core.truth_(ks__18379)) {
      return cljs.core.assoc.call(null, m, k__18378, cljs.core.apply.call(null, update_in, cljs.core._lookup.call(null, m, k__18378, null), ks__18379, f, args))
    }else {
      return cljs.core.assoc.call(null, m, k__18378, cljs.core.apply.call(null, f, cljs.core._lookup.call(null, m, k__18378, null), args))
    }
  };
  var update_in = function(m, p__18371, f, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
    }
    return update_in__delegate.call(this, m, p__18371, f, args)
  };
  update_in.cljs$lang$maxFixedArity = 3;
  update_in.cljs$lang$applyTo = function(arglist__18380) {
    var m = cljs.core.first(arglist__18380);
    var p__18371 = cljs.core.first(cljs.core.next(arglist__18380));
    var f = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18380)));
    var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18380)));
    return update_in__delegate(m, p__18371, f, args)
  };
  update_in.cljs$lang$arity$variadic = update_in__delegate;
  return update_in
}();
goog.provide("cljs.core.Vector");
cljs.core.Vector = function(meta, array, __hash) {
  this.meta = meta;
  this.array = array;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32400159
};
cljs.core.Vector.cljs$lang$type = true;
cljs.core.Vector.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Vector")
};
cljs.core.Vector.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Vector")
};
cljs.core.Vector.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18383 = this;
  var h__2247__auto____18384 = this__18383.__hash;
  if(!(h__2247__auto____18384 == null)) {
    return h__2247__auto____18384
  }else {
    var h__2247__auto____18385 = cljs.core.hash_coll.call(null, coll);
    this__18383.__hash = h__2247__auto____18385;
    return h__2247__auto____18385
  }
};
cljs.core.Vector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18386 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.Vector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18387 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.Vector.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18388 = this;
  var new_array__18389 = this__18388.array.slice();
  new_array__18389[k] = v;
  return new cljs.core.Vector(this__18388.meta, new_array__18389, null)
};
cljs.core.Vector.prototype.call = function() {
  var G__18420 = null;
  var G__18420__2 = function(this_sym18390, k) {
    var this__18392 = this;
    var this_sym18390__18393 = this;
    var coll__18394 = this_sym18390__18393;
    return coll__18394.cljs$core$ILookup$_lookup$arity$2(coll__18394, k)
  };
  var G__18420__3 = function(this_sym18391, k, not_found) {
    var this__18392 = this;
    var this_sym18391__18395 = this;
    var coll__18396 = this_sym18391__18395;
    return coll__18396.cljs$core$ILookup$_lookup$arity$3(coll__18396, k, not_found)
  };
  G__18420 = function(this_sym18391, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18420__2.call(this, this_sym18391, k);
      case 3:
        return G__18420__3.call(this, this_sym18391, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18420
}();
cljs.core.Vector.prototype.apply = function(this_sym18381, args18382) {
  var this__18397 = this;
  return this_sym18381.call.apply(this_sym18381, [this_sym18381].concat(args18382.slice()))
};
cljs.core.Vector.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18398 = this;
  var new_array__18399 = this__18398.array.slice();
  new_array__18399.push(o);
  return new cljs.core.Vector(this__18398.meta, new_array__18399, null)
};
cljs.core.Vector.prototype.toString = function() {
  var this__18400 = this;
  var this__18401 = this;
  return cljs.core.pr_str.call(null, this__18401)
};
cljs.core.Vector.prototype.cljs$core$IReduce$_reduce$arity$2 = function(v, f) {
  var this__18402 = this;
  return cljs.core.ci_reduce.call(null, this__18402.array, f)
};
cljs.core.Vector.prototype.cljs$core$IReduce$_reduce$arity$3 = function(v, f, start) {
  var this__18403 = this;
  return cljs.core.ci_reduce.call(null, this__18403.array, f, start)
};
cljs.core.Vector.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18404 = this;
  if(this__18404.array.length > 0) {
    var vector_seq__18405 = function vector_seq(i) {
      return new cljs.core.LazySeq(null, false, function() {
        if(i < this__18404.array.length) {
          return cljs.core.cons.call(null, this__18404.array[i], vector_seq.call(null, i + 1))
        }else {
          return null
        }
      }, null)
    };
    return vector_seq__18405.call(null, 0)
  }else {
    return null
  }
};
cljs.core.Vector.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18406 = this;
  return this__18406.array.length
};
cljs.core.Vector.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18407 = this;
  var count__18408 = this__18407.array.length;
  if(count__18408 > 0) {
    return this__18407.array[count__18408 - 1]
  }else {
    return null
  }
};
cljs.core.Vector.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18409 = this;
  if(this__18409.array.length > 0) {
    var new_array__18410 = this__18409.array.slice();
    new_array__18410.pop();
    return new cljs.core.Vector(this__18409.meta, new_array__18410, null)
  }else {
    throw new Error("Can't pop empty vector");
  }
};
cljs.core.Vector.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(coll, n, val) {
  var this__18411 = this;
  return coll.cljs$core$IAssociative$_assoc$arity$3(coll, n, val)
};
cljs.core.Vector.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18412 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.Vector.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18413 = this;
  return new cljs.core.Vector(meta, this__18413.array, this__18413.__hash)
};
cljs.core.Vector.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18414 = this;
  return this__18414.meta
};
cljs.core.Vector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18415 = this;
  if(function() {
    var and__3822__auto____18416 = 0 <= n;
    if(and__3822__auto____18416) {
      return n < this__18415.array.length
    }else {
      return and__3822__auto____18416
    }
  }()) {
    return this__18415.array[n]
  }else {
    return null
  }
};
cljs.core.Vector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18417 = this;
  if(function() {
    var and__3822__auto____18418 = 0 <= n;
    if(and__3822__auto____18418) {
      return n < this__18417.array.length
    }else {
      return and__3822__auto____18418
    }
  }()) {
    return this__18417.array[n]
  }else {
    return not_found
  }
};
cljs.core.Vector.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18419 = this;
  return cljs.core.with_meta.call(null, cljs.core.Vector.EMPTY, this__18419.meta)
};
cljs.core.Vector;
cljs.core.Vector.EMPTY = new cljs.core.Vector(null, [], 0);
cljs.core.Vector.fromArray = function(xs) {
  return new cljs.core.Vector(null, xs, null)
};
goog.provide("cljs.core.VectorNode");
cljs.core.VectorNode = function(edit, arr) {
  this.edit = edit;
  this.arr = arr
};
cljs.core.VectorNode.cljs$lang$type = true;
cljs.core.VectorNode.cljs$lang$ctorPrSeq = function(this__2368__auto__) {
  return cljs.core.list.call(null, "cljs.core/VectorNode")
};
cljs.core.VectorNode.cljs$lang$ctorPrWriter = function(this__2368__auto__, writer__2369__auto__) {
  return cljs.core._write.call(null, writer__2369__auto__, "cljs.core/VectorNode")
};
cljs.core.VectorNode;
cljs.core.pv_fresh_node = function pv_fresh_node(edit) {
  return new cljs.core.VectorNode(edit, cljs.core.make_array.call(null, 32))
};
cljs.core.pv_aget = function pv_aget(node, idx) {
  return node.arr[idx]
};
cljs.core.pv_aset = function pv_aset(node, idx, val) {
  return node.arr[idx] = val
};
cljs.core.pv_clone_node = function pv_clone_node(node) {
  return new cljs.core.VectorNode(node.edit, node.arr.slice())
};
cljs.core.tail_off = function tail_off(pv) {
  var cnt__18422 = pv.cnt;
  if(cnt__18422 < 32) {
    return 0
  }else {
    return cnt__18422 - 1 >>> 5 << 5
  }
};
cljs.core.new_path = function new_path(edit, level, node) {
  var ll__18428 = level;
  var ret__18429 = node;
  while(true) {
    if(ll__18428 === 0) {
      return ret__18429
    }else {
      var embed__18430 = ret__18429;
      var r__18431 = cljs.core.pv_fresh_node.call(null, edit);
      var ___18432 = cljs.core.pv_aset.call(null, r__18431, 0, embed__18430);
      var G__18433 = ll__18428 - 5;
      var G__18434 = r__18431;
      ll__18428 = G__18433;
      ret__18429 = G__18434;
      continue
    }
    break
  }
};
cljs.core.push_tail = function push_tail(pv, level, parent, tailnode) {
  var ret__18440 = cljs.core.pv_clone_node.call(null, parent);
  var subidx__18441 = pv.cnt - 1 >>> level & 31;
  if(5 === level) {
    cljs.core.pv_aset.call(null, ret__18440, subidx__18441, tailnode);
    return ret__18440
  }else {
    var child__18442 = cljs.core.pv_aget.call(null, parent, subidx__18441);
    if(!(child__18442 == null)) {
      var node_to_insert__18443 = push_tail.call(null, pv, level - 5, child__18442, tailnode);
      cljs.core.pv_aset.call(null, ret__18440, subidx__18441, node_to_insert__18443);
      return ret__18440
    }else {
      var node_to_insert__18444 = cljs.core.new_path.call(null, null, level - 5, tailnode);
      cljs.core.pv_aset.call(null, ret__18440, subidx__18441, node_to_insert__18444);
      return ret__18440
    }
  }
};
cljs.core.array_for = function array_for(pv, i) {
  if(function() {
    var and__3822__auto____18448 = 0 <= i;
    if(and__3822__auto____18448) {
      return i < pv.cnt
    }else {
      return and__3822__auto____18448
    }
  }()) {
    if(i >= cljs.core.tail_off.call(null, pv)) {
      return pv.tail
    }else {
      var node__18449 = pv.root;
      var level__18450 = pv.shift;
      while(true) {
        if(level__18450 > 0) {
          var G__18451 = cljs.core.pv_aget.call(null, node__18449, i >>> level__18450 & 31);
          var G__18452 = level__18450 - 5;
          node__18449 = G__18451;
          level__18450 = G__18452;
          continue
        }else {
          return node__18449.arr
        }
        break
      }
    }
  }else {
    throw new Error([cljs.core.str("No item "), cljs.core.str(i), cljs.core.str(" in vector of length "), cljs.core.str(pv.cnt)].join(""));
  }
};
cljs.core.do_assoc = function do_assoc(pv, level, node, i, val) {
  var ret__18455 = cljs.core.pv_clone_node.call(null, node);
  if(level === 0) {
    cljs.core.pv_aset.call(null, ret__18455, i & 31, val);
    return ret__18455
  }else {
    var subidx__18456 = i >>> level & 31;
    cljs.core.pv_aset.call(null, ret__18455, subidx__18456, do_assoc.call(null, pv, level - 5, cljs.core.pv_aget.call(null, node, subidx__18456), i, val));
    return ret__18455
  }
};
cljs.core.pop_tail = function pop_tail(pv, level, node) {
  var subidx__18462 = pv.cnt - 2 >>> level & 31;
  if(level > 5) {
    var new_child__18463 = pop_tail.call(null, pv, level - 5, cljs.core.pv_aget.call(null, node, subidx__18462));
    if(function() {
      var and__3822__auto____18464 = new_child__18463 == null;
      if(and__3822__auto____18464) {
        return subidx__18462 === 0
      }else {
        return and__3822__auto____18464
      }
    }()) {
      return null
    }else {
      var ret__18465 = cljs.core.pv_clone_node.call(null, node);
      cljs.core.pv_aset.call(null, ret__18465, subidx__18462, new_child__18463);
      return ret__18465
    }
  }else {
    if(subidx__18462 === 0) {
      return null
    }else {
      if("\ufdd0'else") {
        var ret__18466 = cljs.core.pv_clone_node.call(null, node);
        cljs.core.pv_aset.call(null, ret__18466, subidx__18462, null);
        return ret__18466
      }else {
        return null
      }
    }
  }
};
goog.provide("cljs.core.PersistentVector");
cljs.core.PersistentVector = function(meta, cnt, shift, root, tail, __hash) {
  this.meta = meta;
  this.cnt = cnt;
  this.shift = shift;
  this.root = root;
  this.tail = tail;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 4;
  this.cljs$lang$protocol_mask$partition0$ = 167668511
};
cljs.core.PersistentVector.cljs$lang$type = true;
cljs.core.PersistentVector.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentVector")
};
cljs.core.PersistentVector.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentVector")
};
cljs.core.PersistentVector.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(coll) {
  var this__18469 = this;
  return new cljs.core.TransientVector(this__18469.cnt, this__18469.shift, cljs.core.tv_editable_root.call(null, this__18469.root), cljs.core.tv_editable_tail.call(null, this__18469.tail))
};
cljs.core.PersistentVector.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18470 = this;
  var h__2247__auto____18471 = this__18470.__hash;
  if(!(h__2247__auto____18471 == null)) {
    return h__2247__auto____18471
  }else {
    var h__2247__auto____18472 = cljs.core.hash_coll.call(null, coll);
    this__18470.__hash = h__2247__auto____18472;
    return h__2247__auto____18472
  }
};
cljs.core.PersistentVector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18473 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.PersistentVector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18474 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.PersistentVector.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18475 = this;
  if(function() {
    var and__3822__auto____18476 = 0 <= k;
    if(and__3822__auto____18476) {
      return k < this__18475.cnt
    }else {
      return and__3822__auto____18476
    }
  }()) {
    if(cljs.core.tail_off.call(null, coll) <= k) {
      var new_tail__18477 = this__18475.tail.slice();
      new_tail__18477[k & 31] = v;
      return new cljs.core.PersistentVector(this__18475.meta, this__18475.cnt, this__18475.shift, this__18475.root, new_tail__18477, null)
    }else {
      return new cljs.core.PersistentVector(this__18475.meta, this__18475.cnt, this__18475.shift, cljs.core.do_assoc.call(null, coll, this__18475.shift, this__18475.root, k, v), this__18475.tail, null)
    }
  }else {
    if(k === this__18475.cnt) {
      return coll.cljs$core$ICollection$_conj$arity$2(coll, v)
    }else {
      if("\ufdd0'else") {
        throw new Error([cljs.core.str("Index "), cljs.core.str(k), cljs.core.str(" out of bounds  [0,"), cljs.core.str(this__18475.cnt), cljs.core.str("]")].join(""));
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentVector.prototype.call = function() {
  var G__18525 = null;
  var G__18525__2 = function(this_sym18478, k) {
    var this__18480 = this;
    var this_sym18478__18481 = this;
    var coll__18482 = this_sym18478__18481;
    return coll__18482.cljs$core$ILookup$_lookup$arity$2(coll__18482, k)
  };
  var G__18525__3 = function(this_sym18479, k, not_found) {
    var this__18480 = this;
    var this_sym18479__18483 = this;
    var coll__18484 = this_sym18479__18483;
    return coll__18484.cljs$core$ILookup$_lookup$arity$3(coll__18484, k, not_found)
  };
  G__18525 = function(this_sym18479, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18525__2.call(this, this_sym18479, k);
      case 3:
        return G__18525__3.call(this, this_sym18479, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18525
}();
cljs.core.PersistentVector.prototype.apply = function(this_sym18467, args18468) {
  var this__18485 = this;
  return this_sym18467.call.apply(this_sym18467, [this_sym18467].concat(args18468.slice()))
};
cljs.core.PersistentVector.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(v, f, init) {
  var this__18486 = this;
  var step_init__18487 = [0, init];
  var i__18488 = 0;
  while(true) {
    if(i__18488 < this__18486.cnt) {
      var arr__18489 = cljs.core.array_for.call(null, v, i__18488);
      var len__18490 = arr__18489.length;
      var init__18494 = function() {
        var j__18491 = 0;
        var init__18492 = step_init__18487[1];
        while(true) {
          if(j__18491 < len__18490) {
            var init__18493 = f.call(null, init__18492, j__18491 + i__18488, arr__18489[j__18491]);
            if(cljs.core.reduced_QMARK_.call(null, init__18493)) {
              return init__18493
            }else {
              var G__18526 = j__18491 + 1;
              var G__18527 = init__18493;
              j__18491 = G__18526;
              init__18492 = G__18527;
              continue
            }
          }else {
            step_init__18487[0] = len__18490;
            step_init__18487[1] = init__18492;
            return init__18492
          }
          break
        }
      }();
      if(cljs.core.reduced_QMARK_.call(null, init__18494)) {
        return cljs.core.deref.call(null, init__18494)
      }else {
        var G__18528 = i__18488 + step_init__18487[0];
        i__18488 = G__18528;
        continue
      }
    }else {
      return step_init__18487[1]
    }
    break
  }
};
cljs.core.PersistentVector.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18495 = this;
  if(this__18495.cnt - cljs.core.tail_off.call(null, coll) < 32) {
    var new_tail__18496 = this__18495.tail.slice();
    new_tail__18496.push(o);
    return new cljs.core.PersistentVector(this__18495.meta, this__18495.cnt + 1, this__18495.shift, this__18495.root, new_tail__18496, null)
  }else {
    var root_overflow_QMARK___18497 = this__18495.cnt >>> 5 > 1 << this__18495.shift;
    var new_shift__18498 = root_overflow_QMARK___18497 ? this__18495.shift + 5 : this__18495.shift;
    var new_root__18500 = root_overflow_QMARK___18497 ? function() {
      var n_r__18499 = cljs.core.pv_fresh_node.call(null, null);
      cljs.core.pv_aset.call(null, n_r__18499, 0, this__18495.root);
      cljs.core.pv_aset.call(null, n_r__18499, 1, cljs.core.new_path.call(null, null, this__18495.shift, new cljs.core.VectorNode(null, this__18495.tail)));
      return n_r__18499
    }() : cljs.core.push_tail.call(null, coll, this__18495.shift, this__18495.root, new cljs.core.VectorNode(null, this__18495.tail));
    return new cljs.core.PersistentVector(this__18495.meta, this__18495.cnt + 1, new_shift__18498, new_root__18500, [o], null)
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__18501 = this;
  if(this__18501.cnt > 0) {
    return new cljs.core.RSeq(coll, this__18501.cnt - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IMapEntry$_key$arity$1 = function(coll) {
  var this__18502 = this;
  return coll.cljs$core$IIndexed$_nth$arity$2(coll, 0)
};
cljs.core.PersistentVector.prototype.cljs$core$IMapEntry$_val$arity$1 = function(coll) {
  var this__18503 = this;
  return coll.cljs$core$IIndexed$_nth$arity$2(coll, 1)
};
cljs.core.PersistentVector.prototype.toString = function() {
  var this__18504 = this;
  var this__18505 = this;
  return cljs.core.pr_str.call(null, this__18505)
};
cljs.core.PersistentVector.prototype.cljs$core$IReduce$_reduce$arity$2 = function(v, f) {
  var this__18506 = this;
  return cljs.core.ci_reduce.call(null, v, f)
};
cljs.core.PersistentVector.prototype.cljs$core$IReduce$_reduce$arity$3 = function(v, f, start) {
  var this__18507 = this;
  return cljs.core.ci_reduce.call(null, v, f, start)
};
cljs.core.PersistentVector.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18508 = this;
  if(this__18508.cnt === 0) {
    return null
  }else {
    return cljs.core.chunked_seq.call(null, coll, 0, 0)
  }
};
cljs.core.PersistentVector.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18509 = this;
  return this__18509.cnt
};
cljs.core.PersistentVector.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18510 = this;
  if(this__18510.cnt > 0) {
    return coll.cljs$core$IIndexed$_nth$arity$2(coll, this__18510.cnt - 1)
  }else {
    return null
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18511 = this;
  if(this__18511.cnt === 0) {
    throw new Error("Can't pop empty vector");
  }else {
    if(1 === this__18511.cnt) {
      return cljs.core._with_meta.call(null, cljs.core.PersistentVector.EMPTY, this__18511.meta)
    }else {
      if(1 < this__18511.cnt - cljs.core.tail_off.call(null, coll)) {
        return new cljs.core.PersistentVector(this__18511.meta, this__18511.cnt - 1, this__18511.shift, this__18511.root, this__18511.tail.slice(0, -1), null)
      }else {
        if("\ufdd0'else") {
          var new_tail__18512 = cljs.core.array_for.call(null, coll, this__18511.cnt - 2);
          var nr__18513 = cljs.core.pop_tail.call(null, coll, this__18511.shift, this__18511.root);
          var new_root__18514 = nr__18513 == null ? cljs.core.PersistentVector.EMPTY_NODE : nr__18513;
          var cnt_1__18515 = this__18511.cnt - 1;
          if(function() {
            var and__3822__auto____18516 = 5 < this__18511.shift;
            if(and__3822__auto____18516) {
              return cljs.core.pv_aget.call(null, new_root__18514, 1) == null
            }else {
              return and__3822__auto____18516
            }
          }()) {
            return new cljs.core.PersistentVector(this__18511.meta, cnt_1__18515, this__18511.shift - 5, cljs.core.pv_aget.call(null, new_root__18514, 0), new_tail__18512, null)
          }else {
            return new cljs.core.PersistentVector(this__18511.meta, cnt_1__18515, this__18511.shift, new_root__18514, new_tail__18512, null)
          }
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(coll, n, val) {
  var this__18517 = this;
  return coll.cljs$core$IAssociative$_assoc$arity$3(coll, n, val)
};
cljs.core.PersistentVector.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18518 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentVector.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18519 = this;
  return new cljs.core.PersistentVector(meta, this__18519.cnt, this__18519.shift, this__18519.root, this__18519.tail, this__18519.__hash)
};
cljs.core.PersistentVector.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18520 = this;
  return this__18520.meta
};
cljs.core.PersistentVector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18521 = this;
  return cljs.core.array_for.call(null, coll, n)[n & 31]
};
cljs.core.PersistentVector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18522 = this;
  if(function() {
    var and__3822__auto____18523 = 0 <= n;
    if(and__3822__auto____18523) {
      return n < this__18522.cnt
    }else {
      return and__3822__auto____18523
    }
  }()) {
    return coll.cljs$core$IIndexed$_nth$arity$2(coll, n)
  }else {
    return not_found
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18524 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.EMPTY, this__18524.meta)
};
cljs.core.PersistentVector;
cljs.core.PersistentVector.EMPTY_NODE = cljs.core.pv_fresh_node.call(null, null);
cljs.core.PersistentVector.EMPTY = new cljs.core.PersistentVector(null, 0, 5, cljs.core.PersistentVector.EMPTY_NODE, [], 0);
cljs.core.PersistentVector.fromArray = function(xs, no_clone) {
  var l__18529 = xs.length;
  var xs__18530 = no_clone === true ? xs : xs.slice();
  if(l__18529 < 32) {
    return new cljs.core.PersistentVector(null, l__18529, 5, cljs.core.PersistentVector.EMPTY_NODE, xs__18530, null)
  }else {
    var node__18531 = xs__18530.slice(0, 32);
    var v__18532 = new cljs.core.PersistentVector(null, 32, 5, cljs.core.PersistentVector.EMPTY_NODE, node__18531, null);
    var i__18533 = 32;
    var out__18534 = cljs.core._as_transient.call(null, v__18532);
    while(true) {
      if(i__18533 < l__18529) {
        var G__18535 = i__18533 + 1;
        var G__18536 = cljs.core.conj_BANG_.call(null, out__18534, xs__18530[i__18533]);
        i__18533 = G__18535;
        out__18534 = G__18536;
        continue
      }else {
        return cljs.core.persistent_BANG_.call(null, out__18534)
      }
      break
    }
  }
};
cljs.core.vec = function vec(coll) {
  return cljs.core._persistent_BANG_.call(null, cljs.core.reduce.call(null, cljs.core._conj_BANG_, cljs.core._as_transient.call(null, cljs.core.PersistentVector.EMPTY), coll))
};
cljs.core.vector = function() {
  var vector__delegate = function(args) {
    return cljs.core.vec.call(null, args)
  };
  var vector = function(var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return vector__delegate.call(this, args)
  };
  vector.cljs$lang$maxFixedArity = 0;
  vector.cljs$lang$applyTo = function(arglist__18537) {
    var args = cljs.core.seq(arglist__18537);
    return vector__delegate(args)
  };
  vector.cljs$lang$arity$variadic = vector__delegate;
  return vector
}();
goog.provide("cljs.core.ChunkedSeq");
cljs.core.ChunkedSeq = function(vec, node, i, off, meta, __hash) {
  this.vec = vec;
  this.node = node;
  this.i = i;
  this.off = off;
  this.meta = meta;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition0$ = 31719660;
  this.cljs$lang$protocol_mask$partition1$ = 1536
};
cljs.core.ChunkedSeq.cljs$lang$type = true;
cljs.core.ChunkedSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ChunkedSeq")
};
cljs.core.ChunkedSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ChunkedSeq")
};
cljs.core.ChunkedSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18538 = this;
  var h__2247__auto____18539 = this__18538.__hash;
  if(!(h__2247__auto____18539 == null)) {
    return h__2247__auto____18539
  }else {
    var h__2247__auto____18540 = cljs.core.hash_coll.call(null, coll);
    this__18538.__hash = h__2247__auto____18540;
    return h__2247__auto____18540
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__18541 = this;
  if(this__18541.off + 1 < this__18541.node.length) {
    var s__18542 = cljs.core.chunked_seq.call(null, this__18541.vec, this__18541.node, this__18541.i, this__18541.off + 1);
    if(s__18542 == null) {
      return null
    }else {
      return s__18542
    }
  }else {
    return coll.cljs$core$IChunkedNext$_chunked_next$arity$1(coll)
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18543 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18544 = this;
  return coll
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__18545 = this;
  return this__18545.node[this__18545.off]
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__18546 = this;
  if(this__18546.off + 1 < this__18546.node.length) {
    var s__18547 = cljs.core.chunked_seq.call(null, this__18546.vec, this__18546.node, this__18546.i, this__18546.off + 1);
    if(s__18547 == null) {
      return cljs.core.List.EMPTY
    }else {
      return s__18547
    }
  }else {
    return coll.cljs$core$IChunkedSeq$_chunked_rest$arity$1(coll)
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedNext$_chunked_next$arity$1 = function(coll) {
  var this__18548 = this;
  var l__18549 = this__18548.node.length;
  var s__18550 = this__18548.i + l__18549 < cljs.core._count.call(null, this__18548.vec) ? cljs.core.chunked_seq.call(null, this__18548.vec, this__18548.i + l__18549, 0) : null;
  if(s__18550 == null) {
    return null
  }else {
    return s__18550
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18551 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, m) {
  var this__18552 = this;
  return cljs.core.chunked_seq.call(null, this__18552.vec, this__18552.node, this__18552.i, this__18552.off, m)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IWithMeta$_meta$arity$1 = function(coll) {
  var this__18553 = this;
  return this__18553.meta
};
cljs.core.ChunkedSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18554 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.EMPTY, this__18554.meta)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedSeq$_chunked_first$arity$1 = function(coll) {
  var this__18555 = this;
  return cljs.core.array_chunk.call(null, this__18555.node, this__18555.off)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedSeq$_chunked_rest$arity$1 = function(coll) {
  var this__18556 = this;
  var l__18557 = this__18556.node.length;
  var s__18558 = this__18556.i + l__18557 < cljs.core._count.call(null, this__18556.vec) ? cljs.core.chunked_seq.call(null, this__18556.vec, this__18556.i + l__18557, 0) : null;
  if(s__18558 == null) {
    return cljs.core.List.EMPTY
  }else {
    return s__18558
  }
};
cljs.core.ChunkedSeq;
cljs.core.chunked_seq = function() {
  var chunked_seq = null;
  var chunked_seq__3 = function(vec, i, off) {
    return chunked_seq.call(null, vec, cljs.core.array_for.call(null, vec, i), i, off, null)
  };
  var chunked_seq__4 = function(vec, node, i, off) {
    return chunked_seq.call(null, vec, node, i, off, null)
  };
  var chunked_seq__5 = function(vec, node, i, off, meta) {
    return new cljs.core.ChunkedSeq(vec, node, i, off, meta, null)
  };
  chunked_seq = function(vec, node, i, off, meta) {
    switch(arguments.length) {
      case 3:
        return chunked_seq__3.call(this, vec, node, i);
      case 4:
        return chunked_seq__4.call(this, vec, node, i, off);
      case 5:
        return chunked_seq__5.call(this, vec, node, i, off, meta)
    }
    throw"Invalid arity: " + arguments.length;
  };
  chunked_seq.cljs$lang$arity$3 = chunked_seq__3;
  chunked_seq.cljs$lang$arity$4 = chunked_seq__4;
  chunked_seq.cljs$lang$arity$5 = chunked_seq__5;
  return chunked_seq
}();
goog.provide("cljs.core.Subvec");
cljs.core.Subvec = function(meta, v, start, end, __hash) {
  this.meta = meta;
  this.v = v;
  this.start = start;
  this.end = end;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32400159
};
cljs.core.Subvec.cljs$lang$type = true;
cljs.core.Subvec.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Subvec")
};
cljs.core.Subvec.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Subvec")
};
cljs.core.Subvec.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18561 = this;
  var h__2247__auto____18562 = this__18561.__hash;
  if(!(h__2247__auto____18562 == null)) {
    return h__2247__auto____18562
  }else {
    var h__2247__auto____18563 = cljs.core.hash_coll.call(null, coll);
    this__18561.__hash = h__2247__auto____18563;
    return h__2247__auto____18563
  }
};
cljs.core.Subvec.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18564 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.Subvec.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18565 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.Subvec.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, key, val) {
  var this__18566 = this;
  var v_pos__18567 = this__18566.start + key;
  return new cljs.core.Subvec(this__18566.meta, cljs.core._assoc.call(null, this__18566.v, v_pos__18567, val), this__18566.start, this__18566.end > v_pos__18567 + 1 ? this__18566.end : v_pos__18567 + 1, null)
};
cljs.core.Subvec.prototype.call = function() {
  var G__18593 = null;
  var G__18593__2 = function(this_sym18568, k) {
    var this__18570 = this;
    var this_sym18568__18571 = this;
    var coll__18572 = this_sym18568__18571;
    return coll__18572.cljs$core$ILookup$_lookup$arity$2(coll__18572, k)
  };
  var G__18593__3 = function(this_sym18569, k, not_found) {
    var this__18570 = this;
    var this_sym18569__18573 = this;
    var coll__18574 = this_sym18569__18573;
    return coll__18574.cljs$core$ILookup$_lookup$arity$3(coll__18574, k, not_found)
  };
  G__18593 = function(this_sym18569, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18593__2.call(this, this_sym18569, k);
      case 3:
        return G__18593__3.call(this, this_sym18569, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18593
}();
cljs.core.Subvec.prototype.apply = function(this_sym18559, args18560) {
  var this__18575 = this;
  return this_sym18559.call.apply(this_sym18559, [this_sym18559].concat(args18560.slice()))
};
cljs.core.Subvec.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18576 = this;
  return new cljs.core.Subvec(this__18576.meta, cljs.core._assoc_n.call(null, this__18576.v, this__18576.end, o), this__18576.start, this__18576.end + 1, null)
};
cljs.core.Subvec.prototype.toString = function() {
  var this__18577 = this;
  var this__18578 = this;
  return cljs.core.pr_str.call(null, this__18578)
};
cljs.core.Subvec.prototype.cljs$core$IReduce$_reduce$arity$2 = function(coll, f) {
  var this__18579 = this;
  return cljs.core.ci_reduce.call(null, coll, f)
};
cljs.core.Subvec.prototype.cljs$core$IReduce$_reduce$arity$3 = function(coll, f, start) {
  var this__18580 = this;
  return cljs.core.ci_reduce.call(null, coll, f, start)
};
cljs.core.Subvec.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18581 = this;
  var subvec_seq__18582 = function subvec_seq(i) {
    if(i === this__18581.end) {
      return null
    }else {
      return cljs.core.cons.call(null, cljs.core._nth.call(null, this__18581.v, i), new cljs.core.LazySeq(null, false, function() {
        return subvec_seq.call(null, i + 1)
      }, null))
    }
  };
  return subvec_seq__18582.call(null, this__18581.start)
};
cljs.core.Subvec.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18583 = this;
  return this__18583.end - this__18583.start
};
cljs.core.Subvec.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18584 = this;
  return cljs.core._nth.call(null, this__18584.v, this__18584.end - 1)
};
cljs.core.Subvec.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18585 = this;
  if(this__18585.start === this__18585.end) {
    throw new Error("Can't pop empty vector");
  }else {
    return new cljs.core.Subvec(this__18585.meta, this__18585.v, this__18585.start, this__18585.end - 1, null)
  }
};
cljs.core.Subvec.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(coll, n, val) {
  var this__18586 = this;
  return coll.cljs$core$IAssociative$_assoc$arity$3(coll, n, val)
};
cljs.core.Subvec.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18587 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.Subvec.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18588 = this;
  return new cljs.core.Subvec(meta, this__18588.v, this__18588.start, this__18588.end, this__18588.__hash)
};
cljs.core.Subvec.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18589 = this;
  return this__18589.meta
};
cljs.core.Subvec.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18590 = this;
  return cljs.core._nth.call(null, this__18590.v, this__18590.start + n)
};
cljs.core.Subvec.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18591 = this;
  return cljs.core._nth.call(null, this__18591.v, this__18591.start + n, not_found)
};
cljs.core.Subvec.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18592 = this;
  return cljs.core.with_meta.call(null, cljs.core.Vector.EMPTY, this__18592.meta)
};
cljs.core.Subvec;
cljs.core.subvec = function() {
  var subvec = null;
  var subvec__2 = function(v, start) {
    return subvec.call(null, v, start, cljs.core.count.call(null, v))
  };
  var subvec__3 = function(v, start, end) {
    return new cljs.core.Subvec(null, v, start, end, null)
  };
  subvec = function(v, start, end) {
    switch(arguments.length) {
      case 2:
        return subvec__2.call(this, v, start);
      case 3:
        return subvec__3.call(this, v, start, end)
    }
    throw"Invalid arity: " + arguments.length;
  };
  subvec.cljs$lang$arity$2 = subvec__2;
  subvec.cljs$lang$arity$3 = subvec__3;
  return subvec
}();
cljs.core.tv_ensure_editable = function tv_ensure_editable(edit, node) {
  if(edit === node.edit) {
    return node
  }else {
    return new cljs.core.VectorNode(edit, node.arr.slice())
  }
};
cljs.core.tv_editable_root = function tv_editable_root(node) {
  return new cljs.core.VectorNode({}, node.arr.slice())
};
cljs.core.tv_editable_tail = function tv_editable_tail(tl) {
  var ret__18595 = cljs.core.make_array.call(null, 32);
  cljs.core.array_copy.call(null, tl, 0, ret__18595, 0, tl.length);
  return ret__18595
};
cljs.core.tv_push_tail = function tv_push_tail(tv, level, parent, tail_node) {
  var ret__18599 = cljs.core.tv_ensure_editable.call(null, tv.root.edit, parent);
  var subidx__18600 = tv.cnt - 1 >>> level & 31;
  cljs.core.pv_aset.call(null, ret__18599, subidx__18600, level === 5 ? tail_node : function() {
    var child__18601 = cljs.core.pv_aget.call(null, ret__18599, subidx__18600);
    if(!(child__18601 == null)) {
      return tv_push_tail.call(null, tv, level - 5, child__18601, tail_node)
    }else {
      return cljs.core.new_path.call(null, tv.root.edit, level - 5, tail_node)
    }
  }());
  return ret__18599
};
cljs.core.tv_pop_tail = function tv_pop_tail(tv, level, node) {
  var node__18606 = cljs.core.tv_ensure_editable.call(null, tv.root.edit, node);
  var subidx__18607 = tv.cnt - 2 >>> level & 31;
  if(level > 5) {
    var new_child__18608 = tv_pop_tail.call(null, tv, level - 5, cljs.core.pv_aget.call(null, node__18606, subidx__18607));
    if(function() {
      var and__3822__auto____18609 = new_child__18608 == null;
      if(and__3822__auto____18609) {
        return subidx__18607 === 0
      }else {
        return and__3822__auto____18609
      }
    }()) {
      return null
    }else {
      cljs.core.pv_aset.call(null, node__18606, subidx__18607, new_child__18608);
      return node__18606
    }
  }else {
    if(subidx__18607 === 0) {
      return null
    }else {
      if("\ufdd0'else") {
        cljs.core.pv_aset.call(null, node__18606, subidx__18607, null);
        return node__18606
      }else {
        return null
      }
    }
  }
};
cljs.core.editable_array_for = function editable_array_for(tv, i) {
  if(function() {
    var and__3822__auto____18614 = 0 <= i;
    if(and__3822__auto____18614) {
      return i < tv.cnt
    }else {
      return and__3822__auto____18614
    }
  }()) {
    if(i >= cljs.core.tail_off.call(null, tv)) {
      return tv.tail
    }else {
      var root__18615 = tv.root;
      var node__18616 = root__18615;
      var level__18617 = tv.shift;
      while(true) {
        if(level__18617 > 0) {
          var G__18618 = cljs.core.tv_ensure_editable.call(null, root__18615.edit, cljs.core.pv_aget.call(null, node__18616, i >>> level__18617 & 31));
          var G__18619 = level__18617 - 5;
          node__18616 = G__18618;
          level__18617 = G__18619;
          continue
        }else {
          return node__18616.arr
        }
        break
      }
    }
  }else {
    throw new Error([cljs.core.str("No item "), cljs.core.str(i), cljs.core.str(" in transient vector of length "), cljs.core.str(tv.cnt)].join(""));
  }
};
goog.provide("cljs.core.TransientVector");
cljs.core.TransientVector = function(cnt, shift, root, tail) {
  this.cnt = cnt;
  this.shift = shift;
  this.root = root;
  this.tail = tail;
  this.cljs$lang$protocol_mask$partition0$ = 275;
  this.cljs$lang$protocol_mask$partition1$ = 88
};
cljs.core.TransientVector.cljs$lang$type = true;
cljs.core.TransientVector.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/TransientVector")
};
cljs.core.TransientVector.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/TransientVector")
};
cljs.core.TransientVector.prototype.call = function() {
  var G__18659 = null;
  var G__18659__2 = function(this_sym18622, k) {
    var this__18624 = this;
    var this_sym18622__18625 = this;
    var coll__18626 = this_sym18622__18625;
    return coll__18626.cljs$core$ILookup$_lookup$arity$2(coll__18626, k)
  };
  var G__18659__3 = function(this_sym18623, k, not_found) {
    var this__18624 = this;
    var this_sym18623__18627 = this;
    var coll__18628 = this_sym18623__18627;
    return coll__18628.cljs$core$ILookup$_lookup$arity$3(coll__18628, k, not_found)
  };
  G__18659 = function(this_sym18623, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18659__2.call(this, this_sym18623, k);
      case 3:
        return G__18659__3.call(this, this_sym18623, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18659
}();
cljs.core.TransientVector.prototype.apply = function(this_sym18620, args18621) {
  var this__18629 = this;
  return this_sym18620.call.apply(this_sym18620, [this_sym18620].concat(args18621.slice()))
};
cljs.core.TransientVector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18630 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.TransientVector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18631 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.TransientVector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18632 = this;
  if(this__18632.root.edit) {
    return cljs.core.array_for.call(null, coll, n)[n & 31]
  }else {
    throw new Error("nth after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18633 = this;
  if(function() {
    var and__3822__auto____18634 = 0 <= n;
    if(and__3822__auto____18634) {
      return n < this__18633.cnt
    }else {
      return and__3822__auto____18634
    }
  }()) {
    return coll.cljs$core$IIndexed$_nth$arity$2(coll, n)
  }else {
    return not_found
  }
};
cljs.core.TransientVector.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18635 = this;
  if(this__18635.root.edit) {
    return this__18635.cnt
  }else {
    throw new Error("count after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3 = function(tcoll, n, val) {
  var this__18636 = this;
  if(this__18636.root.edit) {
    if(function() {
      var and__3822__auto____18637 = 0 <= n;
      if(and__3822__auto____18637) {
        return n < this__18636.cnt
      }else {
        return and__3822__auto____18637
      }
    }()) {
      if(cljs.core.tail_off.call(null, tcoll) <= n) {
        this__18636.tail[n & 31] = val;
        return tcoll
      }else {
        var new_root__18642 = function go(level, node) {
          var node__18640 = cljs.core.tv_ensure_editable.call(null, this__18636.root.edit, node);
          if(level === 0) {
            cljs.core.pv_aset.call(null, node__18640, n & 31, val);
            return node__18640
          }else {
            var subidx__18641 = n >>> level & 31;
            cljs.core.pv_aset.call(null, node__18640, subidx__18641, go.call(null, level - 5, cljs.core.pv_aget.call(null, node__18640, subidx__18641)));
            return node__18640
          }
        }.call(null, this__18636.shift, this__18636.root);
        this__18636.root = new_root__18642;
        return tcoll
      }
    }else {
      if(n === this__18636.cnt) {
        return tcoll.cljs$core$ITransientCollection$_conj_BANG_$arity$2(tcoll, val)
      }else {
        if("\ufdd0'else") {
          throw new Error([cljs.core.str("Index "), cljs.core.str(n), cljs.core.str(" out of bounds for TransientVector of length"), cljs.core.str(this__18636.cnt)].join(""));
        }else {
          return null
        }
      }
    }
  }else {
    throw new Error("assoc! after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$ITransientVector$_pop_BANG_$arity$1 = function(tcoll) {
  var this__18643 = this;
  if(this__18643.root.edit) {
    if(this__18643.cnt === 0) {
      throw new Error("Can't pop empty vector");
    }else {
      if(1 === this__18643.cnt) {
        this__18643.cnt = 0;
        return tcoll
      }else {
        if((this__18643.cnt - 1 & 31) > 0) {
          this__18643.cnt = this__18643.cnt - 1;
          return tcoll
        }else {
          if("\ufdd0'else") {
            var new_tail__18644 = cljs.core.editable_array_for.call(null, tcoll, this__18643.cnt - 2);
            var new_root__18646 = function() {
              var nr__18645 = cljs.core.tv_pop_tail.call(null, tcoll, this__18643.shift, this__18643.root);
              if(!(nr__18645 == null)) {
                return nr__18645
              }else {
                return new cljs.core.VectorNode(this__18643.root.edit, cljs.core.make_array.call(null, 32))
              }
            }();
            if(function() {
              var and__3822__auto____18647 = 5 < this__18643.shift;
              if(and__3822__auto____18647) {
                return cljs.core.pv_aget.call(null, new_root__18646, 1) == null
              }else {
                return and__3822__auto____18647
              }
            }()) {
              var new_root__18648 = cljs.core.tv_ensure_editable.call(null, this__18643.root.edit, cljs.core.pv_aget.call(null, new_root__18646, 0));
              this__18643.root = new_root__18648;
              this__18643.shift = this__18643.shift - 5;
              this__18643.cnt = this__18643.cnt - 1;
              this__18643.tail = new_tail__18644;
              return tcoll
            }else {
              this__18643.root = new_root__18646;
              this__18643.cnt = this__18643.cnt - 1;
              this__18643.tail = new_tail__18644;
              return tcoll
            }
          }else {
            return null
          }
        }
      }
    }
  }else {
    throw new Error("pop! after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(tcoll, key, val) {
  var this__18649 = this;
  return tcoll.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3(tcoll, key, val)
};
cljs.core.TransientVector.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, o) {
  var this__18650 = this;
  if(this__18650.root.edit) {
    if(this__18650.cnt - cljs.core.tail_off.call(null, tcoll) < 32) {
      this__18650.tail[this__18650.cnt & 31] = o;
      this__18650.cnt = this__18650.cnt + 1;
      return tcoll
    }else {
      var tail_node__18651 = new cljs.core.VectorNode(this__18650.root.edit, this__18650.tail);
      var new_tail__18652 = cljs.core.make_array.call(null, 32);
      new_tail__18652[0] = o;
      this__18650.tail = new_tail__18652;
      if(this__18650.cnt >>> 5 > 1 << this__18650.shift) {
        var new_root_array__18653 = cljs.core.make_array.call(null, 32);
        var new_shift__18654 = this__18650.shift + 5;
        new_root_array__18653[0] = this__18650.root;
        new_root_array__18653[1] = cljs.core.new_path.call(null, this__18650.root.edit, this__18650.shift, tail_node__18651);
        this__18650.root = new cljs.core.VectorNode(this__18650.root.edit, new_root_array__18653);
        this__18650.shift = new_shift__18654;
        this__18650.cnt = this__18650.cnt + 1;
        return tcoll
      }else {
        var new_root__18655 = cljs.core.tv_push_tail.call(null, tcoll, this__18650.shift, this__18650.root, tail_node__18651);
        this__18650.root = new_root__18655;
        this__18650.cnt = this__18650.cnt + 1;
        return tcoll
      }
    }
  }else {
    throw new Error("conj! after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__18656 = this;
  if(this__18656.root.edit) {
    this__18656.root.edit = null;
    var len__18657 = this__18656.cnt - cljs.core.tail_off.call(null, tcoll);
    var trimmed_tail__18658 = cljs.core.make_array.call(null, len__18657);
    cljs.core.array_copy.call(null, this__18656.tail, 0, trimmed_tail__18658, 0, len__18657);
    return new cljs.core.PersistentVector(null, this__18656.cnt, this__18656.shift, this__18656.root, trimmed_tail__18658, null)
  }else {
    throw new Error("persistent! called twice");
  }
};
cljs.core.TransientVector;
goog.provide("cljs.core.PersistentQueueSeq");
cljs.core.PersistentQueueSeq = function(meta, front, rear, __hash) {
  this.meta = meta;
  this.front = front;
  this.rear = rear;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850572
};
cljs.core.PersistentQueueSeq.cljs$lang$type = true;
cljs.core.PersistentQueueSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentQueueSeq")
};
cljs.core.PersistentQueueSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentQueueSeq")
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18660 = this;
  var h__2247__auto____18661 = this__18660.__hash;
  if(!(h__2247__auto____18661 == null)) {
    return h__2247__auto____18661
  }else {
    var h__2247__auto____18662 = cljs.core.hash_coll.call(null, coll);
    this__18660.__hash = h__2247__auto____18662;
    return h__2247__auto____18662
  }
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18663 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.PersistentQueueSeq.prototype.toString = function() {
  var this__18664 = this;
  var this__18665 = this;
  return cljs.core.pr_str.call(null, this__18665)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18666 = this;
  return coll
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__18667 = this;
  return cljs.core._first.call(null, this__18667.front)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__18668 = this;
  var temp__3971__auto____18669 = cljs.core.next.call(null, this__18668.front);
  if(temp__3971__auto____18669) {
    var f1__18670 = temp__3971__auto____18669;
    return new cljs.core.PersistentQueueSeq(this__18668.meta, f1__18670, this__18668.rear, null)
  }else {
    if(this__18668.rear == null) {
      return coll.cljs$core$IEmptyableCollection$_empty$arity$1(coll)
    }else {
      return new cljs.core.PersistentQueueSeq(this__18668.meta, this__18668.rear, null, null)
    }
  }
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18671 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18672 = this;
  return new cljs.core.PersistentQueueSeq(meta, this__18672.front, this__18672.rear, this__18672.__hash)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18673 = this;
  return this__18673.meta
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18674 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__18674.meta)
};
cljs.core.PersistentQueueSeq;
goog.provide("cljs.core.PersistentQueue");
cljs.core.PersistentQueue = function(meta, count, front, rear, __hash) {
  this.meta = meta;
  this.count = count;
  this.front = front;
  this.rear = rear;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31858766
};
cljs.core.PersistentQueue.cljs$lang$type = true;
cljs.core.PersistentQueue.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentQueue")
};
cljs.core.PersistentQueue.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentQueue")
};
cljs.core.PersistentQueue.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18675 = this;
  var h__2247__auto____18676 = this__18675.__hash;
  if(!(h__2247__auto____18676 == null)) {
    return h__2247__auto____18676
  }else {
    var h__2247__auto____18677 = cljs.core.hash_coll.call(null, coll);
    this__18675.__hash = h__2247__auto____18677;
    return h__2247__auto____18677
  }
};
cljs.core.PersistentQueue.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18678 = this;
  if(cljs.core.truth_(this__18678.front)) {
    return new cljs.core.PersistentQueue(this__18678.meta, this__18678.count + 1, this__18678.front, cljs.core.conj.call(null, function() {
      var or__3824__auto____18679 = this__18678.rear;
      if(cljs.core.truth_(or__3824__auto____18679)) {
        return or__3824__auto____18679
      }else {
        return cljs.core.PersistentVector.EMPTY
      }
    }(), o), null)
  }else {
    return new cljs.core.PersistentQueue(this__18678.meta, this__18678.count + 1, cljs.core.conj.call(null, this__18678.front, o), cljs.core.PersistentVector.EMPTY, null)
  }
};
cljs.core.PersistentQueue.prototype.toString = function() {
  var this__18680 = this;
  var this__18681 = this;
  return cljs.core.pr_str.call(null, this__18681)
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18682 = this;
  var rear__18683 = cljs.core.seq.call(null, this__18682.rear);
  if(cljs.core.truth_(function() {
    var or__3824__auto____18684 = this__18682.front;
    if(cljs.core.truth_(or__3824__auto____18684)) {
      return or__3824__auto____18684
    }else {
      return rear__18683
    }
  }())) {
    return new cljs.core.PersistentQueueSeq(null, this__18682.front, cljs.core.seq.call(null, rear__18683), null)
  }else {
    return null
  }
};
cljs.core.PersistentQueue.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18685 = this;
  return this__18685.count
};
cljs.core.PersistentQueue.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18686 = this;
  return cljs.core._first.call(null, this__18686.front)
};
cljs.core.PersistentQueue.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18687 = this;
  if(cljs.core.truth_(this__18687.front)) {
    var temp__3971__auto____18688 = cljs.core.next.call(null, this__18687.front);
    if(temp__3971__auto____18688) {
      var f1__18689 = temp__3971__auto____18688;
      return new cljs.core.PersistentQueue(this__18687.meta, this__18687.count - 1, f1__18689, this__18687.rear, null)
    }else {
      return new cljs.core.PersistentQueue(this__18687.meta, this__18687.count - 1, cljs.core.seq.call(null, this__18687.rear), cljs.core.PersistentVector.EMPTY, null)
    }
  }else {
    return coll
  }
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__18690 = this;
  return cljs.core.first.call(null, this__18690.front)
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__18691 = this;
  return cljs.core.rest.call(null, cljs.core.seq.call(null, coll))
};
cljs.core.PersistentQueue.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18692 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentQueue.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18693 = this;
  return new cljs.core.PersistentQueue(meta, this__18693.count, this__18693.front, this__18693.rear, this__18693.__hash)
};
cljs.core.PersistentQueue.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18694 = this;
  return this__18694.meta
};
cljs.core.PersistentQueue.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18695 = this;
  return cljs.core.PersistentQueue.EMPTY
};
cljs.core.PersistentQueue;
cljs.core.PersistentQueue.EMPTY = new cljs.core.PersistentQueue(null, 0, null, cljs.core.PersistentVector.EMPTY, 0);
goog.provide("cljs.core.NeverEquiv");
cljs.core.NeverEquiv = function() {
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2097152
};
cljs.core.NeverEquiv.cljs$lang$type = true;
cljs.core.NeverEquiv.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/NeverEquiv")
};
cljs.core.NeverEquiv.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/NeverEquiv")
};
cljs.core.NeverEquiv.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(o, other) {
  var this__18696 = this;
  return false
};
cljs.core.NeverEquiv;
cljs.core.never_equiv = new cljs.core.NeverEquiv;
cljs.core.equiv_map = function equiv_map(x, y) {
  return cljs.core.boolean$.call(null, cljs.core.map_QMARK_.call(null, y) ? cljs.core.count.call(null, x) === cljs.core.count.call(null, y) ? cljs.core.every_QMARK_.call(null, cljs.core.identity, cljs.core.map.call(null, function(xkv) {
    return cljs.core._EQ_.call(null, cljs.core._lookup.call(null, y, cljs.core.first.call(null, xkv), cljs.core.never_equiv), cljs.core.second.call(null, xkv))
  }, x)) : null : null)
};
cljs.core.scan_array = function scan_array(incr, k, array) {
  var len__18699 = array.length;
  var i__18700 = 0;
  while(true) {
    if(i__18700 < len__18699) {
      if(k === array[i__18700]) {
        return i__18700
      }else {
        var G__18701 = i__18700 + incr;
        i__18700 = G__18701;
        continue
      }
    }else {
      return null
    }
    break
  }
};
cljs.core.obj_map_compare_keys = function obj_map_compare_keys(a, b) {
  var a__18704 = cljs.core.hash.call(null, a);
  var b__18705 = cljs.core.hash.call(null, b);
  if(a__18704 < b__18705) {
    return-1
  }else {
    if(a__18704 > b__18705) {
      return 1
    }else {
      if("\ufdd0'else") {
        return 0
      }else {
        return null
      }
    }
  }
};
cljs.core.obj_map__GT_hash_map = function obj_map__GT_hash_map(m, k, v) {
  var ks__18713 = m.keys;
  var len__18714 = ks__18713.length;
  var so__18715 = m.strobj;
  var out__18716 = cljs.core.with_meta.call(null, cljs.core.PersistentHashMap.EMPTY, cljs.core.meta.call(null, m));
  var i__18717 = 0;
  var out__18718 = cljs.core.transient$.call(null, out__18716);
  while(true) {
    if(i__18717 < len__18714) {
      var k__18719 = ks__18713[i__18717];
      var G__18720 = i__18717 + 1;
      var G__18721 = cljs.core.assoc_BANG_.call(null, out__18718, k__18719, so__18715[k__18719]);
      i__18717 = G__18720;
      out__18718 = G__18721;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, cljs.core.assoc_BANG_.call(null, out__18718, k, v))
    }
    break
  }
};
cljs.core.obj_clone = function obj_clone(obj, ks) {
  var new_obj__18727 = {};
  var l__18728 = ks.length;
  var i__18729 = 0;
  while(true) {
    if(i__18729 < l__18728) {
      var k__18730 = ks[i__18729];
      new_obj__18727[k__18730] = obj[k__18730];
      var G__18731 = i__18729 + 1;
      i__18729 = G__18731;
      continue
    }else {
    }
    break
  }
  return new_obj__18727
};
goog.provide("cljs.core.ObjMap");
cljs.core.ObjMap = function(meta, keys, strobj, update_count, __hash) {
  this.meta = meta;
  this.keys = keys;
  this.strobj = strobj;
  this.update_count = update_count;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 4;
  this.cljs$lang$protocol_mask$partition0$ = 15075087
};
cljs.core.ObjMap.cljs$lang$type = true;
cljs.core.ObjMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ObjMap")
};
cljs.core.ObjMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ObjMap")
};
cljs.core.ObjMap.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(coll) {
  var this__18734 = this;
  return cljs.core.transient$.call(null, cljs.core.into.call(null, cljs.core.hash_map.call(null), coll))
};
cljs.core.ObjMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18735 = this;
  var h__2247__auto____18736 = this__18735.__hash;
  if(!(h__2247__auto____18736 == null)) {
    return h__2247__auto____18736
  }else {
    var h__2247__auto____18737 = cljs.core.hash_imap.call(null, coll);
    this__18735.__hash = h__2247__auto____18737;
    return h__2247__auto____18737
  }
};
cljs.core.ObjMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18738 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.ObjMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18739 = this;
  if(function() {
    var and__3822__auto____18740 = goog.isString(k);
    if(and__3822__auto____18740) {
      return!(cljs.core.scan_array.call(null, 1, k, this__18739.keys) == null)
    }else {
      return and__3822__auto____18740
    }
  }()) {
    return this__18739.strobj[k]
  }else {
    return not_found
  }
};
cljs.core.ObjMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18741 = this;
  if(goog.isString(k)) {
    if(function() {
      var or__3824__auto____18742 = this__18741.update_count > cljs.core.ObjMap.HASHMAP_THRESHOLD;
      if(or__3824__auto____18742) {
        return or__3824__auto____18742
      }else {
        return this__18741.keys.length >= cljs.core.ObjMap.HASHMAP_THRESHOLD
      }
    }()) {
      return cljs.core.obj_map__GT_hash_map.call(null, coll, k, v)
    }else {
      if(!(cljs.core.scan_array.call(null, 1, k, this__18741.keys) == null)) {
        var new_strobj__18743 = cljs.core.obj_clone.call(null, this__18741.strobj, this__18741.keys);
        new_strobj__18743[k] = v;
        return new cljs.core.ObjMap(this__18741.meta, this__18741.keys, new_strobj__18743, this__18741.update_count + 1, null)
      }else {
        var new_strobj__18744 = cljs.core.obj_clone.call(null, this__18741.strobj, this__18741.keys);
        var new_keys__18745 = this__18741.keys.slice();
        new_strobj__18744[k] = v;
        new_keys__18745.push(k);
        return new cljs.core.ObjMap(this__18741.meta, new_keys__18745, new_strobj__18744, this__18741.update_count + 1, null)
      }
    }
  }else {
    return cljs.core.obj_map__GT_hash_map.call(null, coll, k, v)
  }
};
cljs.core.ObjMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__18746 = this;
  if(function() {
    var and__3822__auto____18747 = goog.isString(k);
    if(and__3822__auto____18747) {
      return!(cljs.core.scan_array.call(null, 1, k, this__18746.keys) == null)
    }else {
      return and__3822__auto____18747
    }
  }()) {
    return true
  }else {
    return false
  }
};
cljs.core.ObjMap.prototype.call = function() {
  var G__18769 = null;
  var G__18769__2 = function(this_sym18748, k) {
    var this__18750 = this;
    var this_sym18748__18751 = this;
    var coll__18752 = this_sym18748__18751;
    return coll__18752.cljs$core$ILookup$_lookup$arity$2(coll__18752, k)
  };
  var G__18769__3 = function(this_sym18749, k, not_found) {
    var this__18750 = this;
    var this_sym18749__18753 = this;
    var coll__18754 = this_sym18749__18753;
    return coll__18754.cljs$core$ILookup$_lookup$arity$3(coll__18754, k, not_found)
  };
  G__18769 = function(this_sym18749, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18769__2.call(this, this_sym18749, k);
      case 3:
        return G__18769__3.call(this, this_sym18749, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18769
}();
cljs.core.ObjMap.prototype.apply = function(this_sym18732, args18733) {
  var this__18755 = this;
  return this_sym18732.call.apply(this_sym18732, [this_sym18732].concat(args18733.slice()))
};
cljs.core.ObjMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__18756 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.ObjMap.prototype.toString = function() {
  var this__18757 = this;
  var this__18758 = this;
  return cljs.core.pr_str.call(null, this__18758)
};
cljs.core.ObjMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18759 = this;
  if(this__18759.keys.length > 0) {
    return cljs.core.map.call(null, function(p1__18722_SHARP_) {
      return cljs.core.vector.call(null, p1__18722_SHARP_, this__18759.strobj[p1__18722_SHARP_])
    }, this__18759.keys.sort(cljs.core.obj_map_compare_keys))
  }else {
    return null
  }
};
cljs.core.ObjMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18760 = this;
  return this__18760.keys.length
};
cljs.core.ObjMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18761 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.ObjMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18762 = this;
  return new cljs.core.ObjMap(meta, this__18762.keys, this__18762.strobj, this__18762.update_count, this__18762.__hash)
};
cljs.core.ObjMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18763 = this;
  return this__18763.meta
};
cljs.core.ObjMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18764 = this;
  return cljs.core.with_meta.call(null, cljs.core.ObjMap.EMPTY, this__18764.meta)
};
cljs.core.ObjMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__18765 = this;
  if(function() {
    var and__3822__auto____18766 = goog.isString(k);
    if(and__3822__auto____18766) {
      return!(cljs.core.scan_array.call(null, 1, k, this__18765.keys) == null)
    }else {
      return and__3822__auto____18766
    }
  }()) {
    var new_keys__18767 = this__18765.keys.slice();
    var new_strobj__18768 = cljs.core.obj_clone.call(null, this__18765.strobj, this__18765.keys);
    new_keys__18767.splice(cljs.core.scan_array.call(null, 1, k, new_keys__18767), 1);
    cljs.core.js_delete.call(null, new_strobj__18768, k);
    return new cljs.core.ObjMap(this__18765.meta, new_keys__18767, new_strobj__18768, this__18765.update_count + 1, null)
  }else {
    return coll
  }
};
cljs.core.ObjMap;
cljs.core.ObjMap.EMPTY = new cljs.core.ObjMap(null, [], {}, 0, 0);
cljs.core.ObjMap.HASHMAP_THRESHOLD = 32;
cljs.core.ObjMap.fromObject = function(ks, obj) {
  return new cljs.core.ObjMap(null, ks, obj, 0, null)
};
goog.provide("cljs.core.HashMap");
cljs.core.HashMap = function(meta, count, hashobj, __hash) {
  this.meta = meta;
  this.count = count;
  this.hashobj = hashobj;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 15075087
};
cljs.core.HashMap.cljs$lang$type = true;
cljs.core.HashMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/HashMap")
};
cljs.core.HashMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/HashMap")
};
cljs.core.HashMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18773 = this;
  var h__2247__auto____18774 = this__18773.__hash;
  if(!(h__2247__auto____18774 == null)) {
    return h__2247__auto____18774
  }else {
    var h__2247__auto____18775 = cljs.core.hash_imap.call(null, coll);
    this__18773.__hash = h__2247__auto____18775;
    return h__2247__auto____18775
  }
};
cljs.core.HashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18776 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.HashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18777 = this;
  var bucket__18778 = this__18777.hashobj[cljs.core.hash.call(null, k)];
  var i__18779 = cljs.core.truth_(bucket__18778) ? cljs.core.scan_array.call(null, 2, k, bucket__18778) : null;
  if(cljs.core.truth_(i__18779)) {
    return bucket__18778[i__18779 + 1]
  }else {
    return not_found
  }
};
cljs.core.HashMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18780 = this;
  var h__18781 = cljs.core.hash.call(null, k);
  var bucket__18782 = this__18780.hashobj[h__18781];
  if(cljs.core.truth_(bucket__18782)) {
    var new_bucket__18783 = bucket__18782.slice();
    var new_hashobj__18784 = goog.object.clone(this__18780.hashobj);
    new_hashobj__18784[h__18781] = new_bucket__18783;
    var temp__3971__auto____18785 = cljs.core.scan_array.call(null, 2, k, new_bucket__18783);
    if(cljs.core.truth_(temp__3971__auto____18785)) {
      var i__18786 = temp__3971__auto____18785;
      new_bucket__18783[i__18786 + 1] = v;
      return new cljs.core.HashMap(this__18780.meta, this__18780.count, new_hashobj__18784, null)
    }else {
      new_bucket__18783.push(k, v);
      return new cljs.core.HashMap(this__18780.meta, this__18780.count + 1, new_hashobj__18784, null)
    }
  }else {
    var new_hashobj__18787 = goog.object.clone(this__18780.hashobj);
    new_hashobj__18787[h__18781] = [k, v];
    return new cljs.core.HashMap(this__18780.meta, this__18780.count + 1, new_hashobj__18787, null)
  }
};
cljs.core.HashMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__18788 = this;
  var bucket__18789 = this__18788.hashobj[cljs.core.hash.call(null, k)];
  var i__18790 = cljs.core.truth_(bucket__18789) ? cljs.core.scan_array.call(null, 2, k, bucket__18789) : null;
  if(cljs.core.truth_(i__18790)) {
    return true
  }else {
    return false
  }
};
cljs.core.HashMap.prototype.call = function() {
  var G__18815 = null;
  var G__18815__2 = function(this_sym18791, k) {
    var this__18793 = this;
    var this_sym18791__18794 = this;
    var coll__18795 = this_sym18791__18794;
    return coll__18795.cljs$core$ILookup$_lookup$arity$2(coll__18795, k)
  };
  var G__18815__3 = function(this_sym18792, k, not_found) {
    var this__18793 = this;
    var this_sym18792__18796 = this;
    var coll__18797 = this_sym18792__18796;
    return coll__18797.cljs$core$ILookup$_lookup$arity$3(coll__18797, k, not_found)
  };
  G__18815 = function(this_sym18792, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18815__2.call(this, this_sym18792, k);
      case 3:
        return G__18815__3.call(this, this_sym18792, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18815
}();
cljs.core.HashMap.prototype.apply = function(this_sym18771, args18772) {
  var this__18798 = this;
  return this_sym18771.call.apply(this_sym18771, [this_sym18771].concat(args18772.slice()))
};
cljs.core.HashMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__18799 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.HashMap.prototype.toString = function() {
  var this__18800 = this;
  var this__18801 = this;
  return cljs.core.pr_str.call(null, this__18801)
};
cljs.core.HashMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18802 = this;
  if(this__18802.count > 0) {
    var hashes__18803 = cljs.core.js_keys.call(null, this__18802.hashobj).sort();
    return cljs.core.mapcat.call(null, function(p1__18770_SHARP_) {
      return cljs.core.map.call(null, cljs.core.vec, cljs.core.partition.call(null, 2, this__18802.hashobj[p1__18770_SHARP_]))
    }, hashes__18803)
  }else {
    return null
  }
};
cljs.core.HashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18804 = this;
  return this__18804.count
};
cljs.core.HashMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18805 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.HashMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18806 = this;
  return new cljs.core.HashMap(meta, this__18806.count, this__18806.hashobj, this__18806.__hash)
};
cljs.core.HashMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18807 = this;
  return this__18807.meta
};
cljs.core.HashMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18808 = this;
  return cljs.core.with_meta.call(null, cljs.core.HashMap.EMPTY, this__18808.meta)
};
cljs.core.HashMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__18809 = this;
  var h__18810 = cljs.core.hash.call(null, k);
  var bucket__18811 = this__18809.hashobj[h__18810];
  var i__18812 = cljs.core.truth_(bucket__18811) ? cljs.core.scan_array.call(null, 2, k, bucket__18811) : null;
  if(cljs.core.not.call(null, i__18812)) {
    return coll
  }else {
    var new_hashobj__18813 = goog.object.clone(this__18809.hashobj);
    if(3 > bucket__18811.length) {
      cljs.core.js_delete.call(null, new_hashobj__18813, h__18810)
    }else {
      var new_bucket__18814 = bucket__18811.slice();
      new_bucket__18814.splice(i__18812, 2);
      new_hashobj__18813[h__18810] = new_bucket__18814
    }
    return new cljs.core.HashMap(this__18809.meta, this__18809.count - 1, new_hashobj__18813, null)
  }
};
cljs.core.HashMap;
cljs.core.HashMap.EMPTY = new cljs.core.HashMap(null, 0, {}, 0);
cljs.core.HashMap.fromArrays = function(ks, vs) {
  var len__18816 = ks.length;
  var i__18817 = 0;
  var out__18818 = cljs.core.HashMap.EMPTY;
  while(true) {
    if(i__18817 < len__18816) {
      var G__18819 = i__18817 + 1;
      var G__18820 = cljs.core.assoc.call(null, out__18818, ks[i__18817], vs[i__18817]);
      i__18817 = G__18819;
      out__18818 = G__18820;
      continue
    }else {
      return out__18818
    }
    break
  }
};
cljs.core.array_map_index_of = function array_map_index_of(m, k) {
  var arr__18824 = m.arr;
  var len__18825 = arr__18824.length;
  var i__18826 = 0;
  while(true) {
    if(len__18825 <= i__18826) {
      return-1
    }else {
      if(cljs.core._EQ_.call(null, arr__18824[i__18826], k)) {
        return i__18826
      }else {
        if("\ufdd0'else") {
          var G__18827 = i__18826 + 2;
          i__18826 = G__18827;
          continue
        }else {
          return null
        }
      }
    }
    break
  }
};
goog.provide("cljs.core.PersistentArrayMap");
cljs.core.PersistentArrayMap = function(meta, cnt, arr, __hash) {
  this.meta = meta;
  this.cnt = cnt;
  this.arr = arr;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 4;
  this.cljs$lang$protocol_mask$partition0$ = 16123663
};
cljs.core.PersistentArrayMap.cljs$lang$type = true;
cljs.core.PersistentArrayMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentArrayMap")
};
cljs.core.PersistentArrayMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentArrayMap")
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(coll) {
  var this__18830 = this;
  return new cljs.core.TransientArrayMap({}, this__18830.arr.length, this__18830.arr.slice())
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18831 = this;
  var h__2247__auto____18832 = this__18831.__hash;
  if(!(h__2247__auto____18832 == null)) {
    return h__2247__auto____18832
  }else {
    var h__2247__auto____18833 = cljs.core.hash_imap.call(null, coll);
    this__18831.__hash = h__2247__auto____18833;
    return h__2247__auto____18833
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18834 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18835 = this;
  var idx__18836 = cljs.core.array_map_index_of.call(null, coll, k);
  if(idx__18836 === -1) {
    return not_found
  }else {
    return this__18835.arr[idx__18836 + 1]
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18837 = this;
  var idx__18838 = cljs.core.array_map_index_of.call(null, coll, k);
  if(idx__18838 === -1) {
    if(this__18837.cnt < cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD) {
      return new cljs.core.PersistentArrayMap(this__18837.meta, this__18837.cnt + 1, function() {
        var G__18839__18840 = this__18837.arr.slice();
        G__18839__18840.push(k);
        G__18839__18840.push(v);
        return G__18839__18840
      }(), null)
    }else {
      return cljs.core.persistent_BANG_.call(null, cljs.core.assoc_BANG_.call(null, cljs.core.transient$.call(null, cljs.core.into.call(null, cljs.core.PersistentHashMap.EMPTY, coll)), k, v))
    }
  }else {
    if(v === this__18837.arr[idx__18838 + 1]) {
      return coll
    }else {
      if("\ufdd0'else") {
        return new cljs.core.PersistentArrayMap(this__18837.meta, this__18837.cnt, function() {
          var G__18841__18842 = this__18837.arr.slice();
          G__18841__18842[idx__18838 + 1] = v;
          return G__18841__18842
        }(), null)
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__18843 = this;
  return!(cljs.core.array_map_index_of.call(null, coll, k) === -1)
};
cljs.core.PersistentArrayMap.prototype.call = function() {
  var G__18875 = null;
  var G__18875__2 = function(this_sym18844, k) {
    var this__18846 = this;
    var this_sym18844__18847 = this;
    var coll__18848 = this_sym18844__18847;
    return coll__18848.cljs$core$ILookup$_lookup$arity$2(coll__18848, k)
  };
  var G__18875__3 = function(this_sym18845, k, not_found) {
    var this__18846 = this;
    var this_sym18845__18849 = this;
    var coll__18850 = this_sym18845__18849;
    return coll__18850.cljs$core$ILookup$_lookup$arity$3(coll__18850, k, not_found)
  };
  G__18875 = function(this_sym18845, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18875__2.call(this, this_sym18845, k);
      case 3:
        return G__18875__3.call(this, this_sym18845, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18875
}();
cljs.core.PersistentArrayMap.prototype.apply = function(this_sym18828, args18829) {
  var this__18851 = this;
  return this_sym18828.call.apply(this_sym18828, [this_sym18828].concat(args18829.slice()))
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(coll, f, init) {
  var this__18852 = this;
  var len__18853 = this__18852.arr.length;
  var i__18854 = 0;
  var init__18855 = init;
  while(true) {
    if(i__18854 < len__18853) {
      var init__18856 = f.call(null, init__18855, this__18852.arr[i__18854], this__18852.arr[i__18854 + 1]);
      if(cljs.core.reduced_QMARK_.call(null, init__18856)) {
        return cljs.core.deref.call(null, init__18856)
      }else {
        var G__18876 = i__18854 + 2;
        var G__18877 = init__18856;
        i__18854 = G__18876;
        init__18855 = G__18877;
        continue
      }
    }else {
      return null
    }
    break
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__18857 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.PersistentArrayMap.prototype.toString = function() {
  var this__18858 = this;
  var this__18859 = this;
  return cljs.core.pr_str.call(null, this__18859)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18860 = this;
  if(this__18860.cnt > 0) {
    var len__18861 = this__18860.arr.length;
    var array_map_seq__18862 = function array_map_seq(i) {
      return new cljs.core.LazySeq(null, false, function() {
        if(i < len__18861) {
          return cljs.core.cons.call(null, cljs.core.PersistentVector.fromArray([this__18860.arr[i], this__18860.arr[i + 1]], true), array_map_seq.call(null, i + 2))
        }else {
          return null
        }
      }, null)
    };
    return array_map_seq__18862.call(null, 0)
  }else {
    return null
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18863 = this;
  return this__18863.cnt
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18864 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18865 = this;
  return new cljs.core.PersistentArrayMap(meta, this__18865.cnt, this__18865.arr, this__18865.__hash)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18866 = this;
  return this__18866.meta
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18867 = this;
  return cljs.core._with_meta.call(null, cljs.core.PersistentArrayMap.EMPTY, this__18867.meta)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__18868 = this;
  var idx__18869 = cljs.core.array_map_index_of.call(null, coll, k);
  if(idx__18869 >= 0) {
    var len__18870 = this__18868.arr.length;
    var new_len__18871 = len__18870 - 2;
    if(new_len__18871 === 0) {
      return coll.cljs$core$IEmptyableCollection$_empty$arity$1(coll)
    }else {
      var new_arr__18872 = cljs.core.make_array.call(null, new_len__18871);
      var s__18873 = 0;
      var d__18874 = 0;
      while(true) {
        if(s__18873 >= len__18870) {
          return new cljs.core.PersistentArrayMap(this__18868.meta, this__18868.cnt - 1, new_arr__18872, null)
        }else {
          if(cljs.core._EQ_.call(null, k, this__18868.arr[s__18873])) {
            var G__18878 = s__18873 + 2;
            var G__18879 = d__18874;
            s__18873 = G__18878;
            d__18874 = G__18879;
            continue
          }else {
            if("\ufdd0'else") {
              new_arr__18872[d__18874] = this__18868.arr[s__18873];
              new_arr__18872[d__18874 + 1] = this__18868.arr[s__18873 + 1];
              var G__18880 = s__18873 + 2;
              var G__18881 = d__18874 + 2;
              s__18873 = G__18880;
              d__18874 = G__18881;
              continue
            }else {
              return null
            }
          }
        }
        break
      }
    }
  }else {
    return coll
  }
};
cljs.core.PersistentArrayMap;
cljs.core.PersistentArrayMap.EMPTY = new cljs.core.PersistentArrayMap(null, 0, [], null);
cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD = 16;
cljs.core.PersistentArrayMap.fromArrays = function(ks, vs) {
  var len__18882 = cljs.core.count.call(null, ks);
  var i__18883 = 0;
  var out__18884 = cljs.core.transient$.call(null, cljs.core.PersistentArrayMap.EMPTY);
  while(true) {
    if(i__18883 < len__18882) {
      var G__18885 = i__18883 + 1;
      var G__18886 = cljs.core.assoc_BANG_.call(null, out__18884, ks[i__18883], vs[i__18883]);
      i__18883 = G__18885;
      out__18884 = G__18886;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, out__18884)
    }
    break
  }
};
goog.provide("cljs.core.TransientArrayMap");
cljs.core.TransientArrayMap = function(editable_QMARK_, len, arr) {
  this.editable_QMARK_ = editable_QMARK_;
  this.len = len;
  this.arr = arr;
  this.cljs$lang$protocol_mask$partition1$ = 56;
  this.cljs$lang$protocol_mask$partition0$ = 258
};
cljs.core.TransientArrayMap.cljs$lang$type = true;
cljs.core.TransientArrayMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/TransientArrayMap")
};
cljs.core.TransientArrayMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/TransientArrayMap")
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientMap$_dissoc_BANG_$arity$2 = function(tcoll, key) {
  var this__18887 = this;
  if(cljs.core.truth_(this__18887.editable_QMARK_)) {
    var idx__18888 = cljs.core.array_map_index_of.call(null, tcoll, key);
    if(idx__18888 >= 0) {
      this__18887.arr[idx__18888] = this__18887.arr[this__18887.len - 2];
      this__18887.arr[idx__18888 + 1] = this__18887.arr[this__18887.len - 1];
      var G__18889__18890 = this__18887.arr;
      G__18889__18890.pop();
      G__18889__18890.pop();
      G__18889__18890;
      this__18887.len = this__18887.len - 2
    }else {
    }
    return tcoll
  }else {
    throw new Error("dissoc! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(tcoll, key, val) {
  var this__18891 = this;
  if(cljs.core.truth_(this__18891.editable_QMARK_)) {
    var idx__18892 = cljs.core.array_map_index_of.call(null, tcoll, key);
    if(idx__18892 === -1) {
      if(this__18891.len + 2 <= 2 * cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD) {
        this__18891.len = this__18891.len + 2;
        this__18891.arr.push(key);
        this__18891.arr.push(val);
        return tcoll
      }else {
        return cljs.core.assoc_BANG_.call(null, cljs.core.array__GT_transient_hash_map.call(null, this__18891.len, this__18891.arr), key, val)
      }
    }else {
      if(val === this__18891.arr[idx__18892 + 1]) {
        return tcoll
      }else {
        this__18891.arr[idx__18892 + 1] = val;
        return tcoll
      }
    }
  }else {
    throw new Error("assoc! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, o) {
  var this__18893 = this;
  if(cljs.core.truth_(this__18893.editable_QMARK_)) {
    if(function() {
      var G__18894__18895 = o;
      if(G__18894__18895) {
        if(function() {
          var or__3824__auto____18896 = G__18894__18895.cljs$lang$protocol_mask$partition0$ & 2048;
          if(or__3824__auto____18896) {
            return or__3824__auto____18896
          }else {
            return G__18894__18895.cljs$core$IMapEntry$
          }
        }()) {
          return true
        }else {
          if(!G__18894__18895.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__18894__18895)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__18894__18895)
      }
    }()) {
      return tcoll.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(tcoll, cljs.core.key.call(null, o), cljs.core.val.call(null, o))
    }else {
      var es__18897 = cljs.core.seq.call(null, o);
      var tcoll__18898 = tcoll;
      while(true) {
        var temp__3971__auto____18899 = cljs.core.first.call(null, es__18897);
        if(cljs.core.truth_(temp__3971__auto____18899)) {
          var e__18900 = temp__3971__auto____18899;
          var G__18906 = cljs.core.next.call(null, es__18897);
          var G__18907 = tcoll__18898.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(tcoll__18898, cljs.core.key.call(null, e__18900), cljs.core.val.call(null, e__18900));
          es__18897 = G__18906;
          tcoll__18898 = G__18907;
          continue
        }else {
          return tcoll__18898
        }
        break
      }
    }
  }else {
    throw new Error("conj! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__18901 = this;
  if(cljs.core.truth_(this__18901.editable_QMARK_)) {
    this__18901.editable_QMARK_ = false;
    return new cljs.core.PersistentArrayMap(null, cljs.core.quot.call(null, this__18901.len, 2), this__18901.arr, null)
  }else {
    throw new Error("persistent! called twice");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(tcoll, k) {
  var this__18902 = this;
  return tcoll.cljs$core$ILookup$_lookup$arity$3(tcoll, k, null)
};
cljs.core.TransientArrayMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(tcoll, k, not_found) {
  var this__18903 = this;
  if(cljs.core.truth_(this__18903.editable_QMARK_)) {
    var idx__18904 = cljs.core.array_map_index_of.call(null, tcoll, k);
    if(idx__18904 === -1) {
      return not_found
    }else {
      return this__18903.arr[idx__18904 + 1]
    }
  }else {
    throw new Error("lookup after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ICounted$_count$arity$1 = function(tcoll) {
  var this__18905 = this;
  if(cljs.core.truth_(this__18905.editable_QMARK_)) {
    return cljs.core.quot.call(null, this__18905.len, 2)
  }else {
    throw new Error("count after persistent!");
  }
};
cljs.core.TransientArrayMap;
cljs.core.array__GT_transient_hash_map = function array__GT_transient_hash_map(len, arr) {
  var out__18910 = cljs.core.transient$.call(null, cljs.core.ObjMap.EMPTY);
  var i__18911 = 0;
  while(true) {
    if(i__18911 < len) {
      var G__18912 = cljs.core.assoc_BANG_.call(null, out__18910, arr[i__18911], arr[i__18911 + 1]);
      var G__18913 = i__18911 + 2;
      out__18910 = G__18912;
      i__18911 = G__18913;
      continue
    }else {
      return out__18910
    }
    break
  }
};
goog.provide("cljs.core.Box");
cljs.core.Box = function(val) {
  this.val = val
};
cljs.core.Box.cljs$lang$type = true;
cljs.core.Box.cljs$lang$ctorPrSeq = function(this__2368__auto__) {
  return cljs.core.list.call(null, "cljs.core/Box")
};
cljs.core.Box.cljs$lang$ctorPrWriter = function(this__2368__auto__, writer__2369__auto__) {
  return cljs.core._write.call(null, writer__2369__auto__, "cljs.core/Box")
};
cljs.core.Box;
cljs.core.key_test = function key_test(key, other) {
  if(goog.isString(key)) {
    return key === other
  }else {
    return cljs.core._EQ_.call(null, key, other)
  }
};
cljs.core.mask = function mask(hash, shift) {
  return hash >>> shift & 31
};
cljs.core.clone_and_set = function() {
  var clone_and_set = null;
  var clone_and_set__3 = function(arr, i, a) {
    var G__18918__18919 = arr.slice();
    G__18918__18919[i] = a;
    return G__18918__18919
  };
  var clone_and_set__5 = function(arr, i, a, j, b) {
    var G__18920__18921 = arr.slice();
    G__18920__18921[i] = a;
    G__18920__18921[j] = b;
    return G__18920__18921
  };
  clone_and_set = function(arr, i, a, j, b) {
    switch(arguments.length) {
      case 3:
        return clone_and_set__3.call(this, arr, i, a);
      case 5:
        return clone_and_set__5.call(this, arr, i, a, j, b)
    }
    throw"Invalid arity: " + arguments.length;
  };
  clone_and_set.cljs$lang$arity$3 = clone_and_set__3;
  clone_and_set.cljs$lang$arity$5 = clone_and_set__5;
  return clone_and_set
}();
cljs.core.remove_pair = function remove_pair(arr, i) {
  var new_arr__18923 = cljs.core.make_array.call(null, arr.length - 2);
  cljs.core.array_copy.call(null, arr, 0, new_arr__18923, 0, 2 * i);
  cljs.core.array_copy.call(null, arr, 2 * (i + 1), new_arr__18923, 2 * i, new_arr__18923.length - 2 * i);
  return new_arr__18923
};
cljs.core.bitmap_indexed_node_index = function bitmap_indexed_node_index(bitmap, bit) {
  return cljs.core.bit_count.call(null, bitmap & bit - 1)
};
cljs.core.bitpos = function bitpos(hash, shift) {
  return 1 << (hash >>> shift & 31)
};
cljs.core.edit_and_set = function() {
  var edit_and_set = null;
  var edit_and_set__4 = function(inode, edit, i, a) {
    var editable__18926 = inode.ensure_editable(edit);
    editable__18926.arr[i] = a;
    return editable__18926
  };
  var edit_and_set__6 = function(inode, edit, i, a, j, b) {
    var editable__18927 = inode.ensure_editable(edit);
    editable__18927.arr[i] = a;
    editable__18927.arr[j] = b;
    return editable__18927
  };
  edit_and_set = function(inode, edit, i, a, j, b) {
    switch(arguments.length) {
      case 4:
        return edit_and_set__4.call(this, inode, edit, i, a);
      case 6:
        return edit_and_set__6.call(this, inode, edit, i, a, j, b)
    }
    throw"Invalid arity: " + arguments.length;
  };
  edit_and_set.cljs$lang$arity$4 = edit_and_set__4;
  edit_and_set.cljs$lang$arity$6 = edit_and_set__6;
  return edit_and_set
}();
cljs.core.inode_kv_reduce = function inode_kv_reduce(arr, f, init) {
  var len__18934 = arr.length;
  var i__18935 = 0;
  var init__18936 = init;
  while(true) {
    if(i__18935 < len__18934) {
      var init__18939 = function() {
        var k__18937 = arr[i__18935];
        if(!(k__18937 == null)) {
          return f.call(null, init__18936, k__18937, arr[i__18935 + 1])
        }else {
          var node__18938 = arr[i__18935 + 1];
          if(!(node__18938 == null)) {
            return node__18938.kv_reduce(f, init__18936)
          }else {
            return init__18936
          }
        }
      }();
      if(cljs.core.reduced_QMARK_.call(null, init__18939)) {
        return cljs.core.deref.call(null, init__18939)
      }else {
        var G__18940 = i__18935 + 2;
        var G__18941 = init__18939;
        i__18935 = G__18940;
        init__18936 = G__18941;
        continue
      }
    }else {
      return init__18936
    }
    break
  }
};
goog.provide("cljs.core.BitmapIndexedNode");
cljs.core.BitmapIndexedNode = function(edit, bitmap, arr) {
  this.edit = edit;
  this.bitmap = bitmap;
  this.arr = arr
};
cljs.core.BitmapIndexedNode.cljs$lang$type = true;
cljs.core.BitmapIndexedNode.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/BitmapIndexedNode")
};
cljs.core.BitmapIndexedNode.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/BitmapIndexedNode")
};
cljs.core.BitmapIndexedNode.prototype.edit_and_remove_pair = function(e, bit, i) {
  var this__18942 = this;
  var inode__18943 = this;
  if(this__18942.bitmap === bit) {
    return null
  }else {
    var editable__18944 = inode__18943.ensure_editable(e);
    var earr__18945 = editable__18944.arr;
    var len__18946 = earr__18945.length;
    editable__18944.bitmap = bit ^ editable__18944.bitmap;
    cljs.core.array_copy.call(null, earr__18945, 2 * (i + 1), earr__18945, 2 * i, len__18946 - 2 * (i + 1));
    earr__18945[len__18946 - 2] = null;
    earr__18945[len__18946 - 1] = null;
    return editable__18944
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_assoc_BANG_ = function(edit, shift, hash, key, val, added_leaf_QMARK_) {
  var this__18947 = this;
  var inode__18948 = this;
  var bit__18949 = 1 << (hash >>> shift & 31);
  var idx__18950 = cljs.core.bitmap_indexed_node_index.call(null, this__18947.bitmap, bit__18949);
  if((this__18947.bitmap & bit__18949) === 0) {
    var n__18951 = cljs.core.bit_count.call(null, this__18947.bitmap);
    if(2 * n__18951 < this__18947.arr.length) {
      var editable__18952 = inode__18948.ensure_editable(edit);
      var earr__18953 = editable__18952.arr;
      added_leaf_QMARK_.val = true;
      cljs.core.array_copy_downward.call(null, earr__18953, 2 * idx__18950, earr__18953, 2 * (idx__18950 + 1), 2 * (n__18951 - idx__18950));
      earr__18953[2 * idx__18950] = key;
      earr__18953[2 * idx__18950 + 1] = val;
      editable__18952.bitmap = editable__18952.bitmap | bit__18949;
      return editable__18952
    }else {
      if(n__18951 >= 16) {
        var nodes__18954 = cljs.core.make_array.call(null, 32);
        var jdx__18955 = hash >>> shift & 31;
        nodes__18954[jdx__18955] = cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_);
        var i__18956 = 0;
        var j__18957 = 0;
        while(true) {
          if(i__18956 < 32) {
            if((this__18947.bitmap >>> i__18956 & 1) === 0) {
              var G__19010 = i__18956 + 1;
              var G__19011 = j__18957;
              i__18956 = G__19010;
              j__18957 = G__19011;
              continue
            }else {
              nodes__18954[i__18956] = !(this__18947.arr[j__18957] == null) ? cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift + 5, cljs.core.hash.call(null, this__18947.arr[j__18957]), this__18947.arr[j__18957], this__18947.arr[j__18957 + 1], added_leaf_QMARK_) : this__18947.arr[j__18957 + 1];
              var G__19012 = i__18956 + 1;
              var G__19013 = j__18957 + 2;
              i__18956 = G__19012;
              j__18957 = G__19013;
              continue
            }
          }else {
          }
          break
        }
        return new cljs.core.ArrayNode(edit, n__18951 + 1, nodes__18954)
      }else {
        if("\ufdd0'else") {
          var new_arr__18958 = cljs.core.make_array.call(null, 2 * (n__18951 + 4));
          cljs.core.array_copy.call(null, this__18947.arr, 0, new_arr__18958, 0, 2 * idx__18950);
          new_arr__18958[2 * idx__18950] = key;
          new_arr__18958[2 * idx__18950 + 1] = val;
          cljs.core.array_copy.call(null, this__18947.arr, 2 * idx__18950, new_arr__18958, 2 * (idx__18950 + 1), 2 * (n__18951 - idx__18950));
          added_leaf_QMARK_.val = true;
          var editable__18959 = inode__18948.ensure_editable(edit);
          editable__18959.arr = new_arr__18958;
          editable__18959.bitmap = editable__18959.bitmap | bit__18949;
          return editable__18959
        }else {
          return null
        }
      }
    }
  }else {
    var key_or_nil__18960 = this__18947.arr[2 * idx__18950];
    var val_or_node__18961 = this__18947.arr[2 * idx__18950 + 1];
    if(key_or_nil__18960 == null) {
      var n__18962 = val_or_node__18961.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_);
      if(n__18962 === val_or_node__18961) {
        return inode__18948
      }else {
        return cljs.core.edit_and_set.call(null, inode__18948, edit, 2 * idx__18950 + 1, n__18962)
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__18960)) {
        if(val === val_or_node__18961) {
          return inode__18948
        }else {
          return cljs.core.edit_and_set.call(null, inode__18948, edit, 2 * idx__18950 + 1, val)
        }
      }else {
        if("\ufdd0'else") {
          added_leaf_QMARK_.val = true;
          return cljs.core.edit_and_set.call(null, inode__18948, edit, 2 * idx__18950, null, 2 * idx__18950 + 1, cljs.core.create_node.call(null, edit, shift + 5, key_or_nil__18960, val_or_node__18961, hash, key, val))
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_seq = function() {
  var this__18963 = this;
  var inode__18964 = this;
  return cljs.core.create_inode_seq.call(null, this__18963.arr)
};
cljs.core.BitmapIndexedNode.prototype.inode_without_BANG_ = function(edit, shift, hash, key, removed_leaf_QMARK_) {
  var this__18965 = this;
  var inode__18966 = this;
  var bit__18967 = 1 << (hash >>> shift & 31);
  if((this__18965.bitmap & bit__18967) === 0) {
    return inode__18966
  }else {
    var idx__18968 = cljs.core.bitmap_indexed_node_index.call(null, this__18965.bitmap, bit__18967);
    var key_or_nil__18969 = this__18965.arr[2 * idx__18968];
    var val_or_node__18970 = this__18965.arr[2 * idx__18968 + 1];
    if(key_or_nil__18969 == null) {
      var n__18971 = val_or_node__18970.inode_without_BANG_(edit, shift + 5, hash, key, removed_leaf_QMARK_);
      if(n__18971 === val_or_node__18970) {
        return inode__18966
      }else {
        if(!(n__18971 == null)) {
          return cljs.core.edit_and_set.call(null, inode__18966, edit, 2 * idx__18968 + 1, n__18971)
        }else {
          if(this__18965.bitmap === bit__18967) {
            return null
          }else {
            if("\ufdd0'else") {
              return inode__18966.edit_and_remove_pair(edit, bit__18967, idx__18968)
            }else {
              return null
            }
          }
        }
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__18969)) {
        removed_leaf_QMARK_[0] = true;
        return inode__18966.edit_and_remove_pair(edit, bit__18967, idx__18968)
      }else {
        if("\ufdd0'else") {
          return inode__18966
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.ensure_editable = function(e) {
  var this__18972 = this;
  var inode__18973 = this;
  if(e === this__18972.edit) {
    return inode__18973
  }else {
    var n__18974 = cljs.core.bit_count.call(null, this__18972.bitmap);
    var new_arr__18975 = cljs.core.make_array.call(null, n__18974 < 0 ? 4 : 2 * (n__18974 + 1));
    cljs.core.array_copy.call(null, this__18972.arr, 0, new_arr__18975, 0, 2 * n__18974);
    return new cljs.core.BitmapIndexedNode(e, this__18972.bitmap, new_arr__18975)
  }
};
cljs.core.BitmapIndexedNode.prototype.kv_reduce = function(f, init) {
  var this__18976 = this;
  var inode__18977 = this;
  return cljs.core.inode_kv_reduce.call(null, this__18976.arr, f, init)
};
cljs.core.BitmapIndexedNode.prototype.inode_find = function(shift, hash, key, not_found) {
  var this__18978 = this;
  var inode__18979 = this;
  var bit__18980 = 1 << (hash >>> shift & 31);
  if((this__18978.bitmap & bit__18980) === 0) {
    return not_found
  }else {
    var idx__18981 = cljs.core.bitmap_indexed_node_index.call(null, this__18978.bitmap, bit__18980);
    var key_or_nil__18982 = this__18978.arr[2 * idx__18981];
    var val_or_node__18983 = this__18978.arr[2 * idx__18981 + 1];
    if(key_or_nil__18982 == null) {
      return val_or_node__18983.inode_find(shift + 5, hash, key, not_found)
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__18982)) {
        return cljs.core.PersistentVector.fromArray([key_or_nil__18982, val_or_node__18983], true)
      }else {
        if("\ufdd0'else") {
          return not_found
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_without = function(shift, hash, key) {
  var this__18984 = this;
  var inode__18985 = this;
  var bit__18986 = 1 << (hash >>> shift & 31);
  if((this__18984.bitmap & bit__18986) === 0) {
    return inode__18985
  }else {
    var idx__18987 = cljs.core.bitmap_indexed_node_index.call(null, this__18984.bitmap, bit__18986);
    var key_or_nil__18988 = this__18984.arr[2 * idx__18987];
    var val_or_node__18989 = this__18984.arr[2 * idx__18987 + 1];
    if(key_or_nil__18988 == null) {
      var n__18990 = val_or_node__18989.inode_without(shift + 5, hash, key);
      if(n__18990 === val_or_node__18989) {
        return inode__18985
      }else {
        if(!(n__18990 == null)) {
          return new cljs.core.BitmapIndexedNode(null, this__18984.bitmap, cljs.core.clone_and_set.call(null, this__18984.arr, 2 * idx__18987 + 1, n__18990))
        }else {
          if(this__18984.bitmap === bit__18986) {
            return null
          }else {
            if("\ufdd0'else") {
              return new cljs.core.BitmapIndexedNode(null, this__18984.bitmap ^ bit__18986, cljs.core.remove_pair.call(null, this__18984.arr, idx__18987))
            }else {
              return null
            }
          }
        }
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__18988)) {
        return new cljs.core.BitmapIndexedNode(null, this__18984.bitmap ^ bit__18986, cljs.core.remove_pair.call(null, this__18984.arr, idx__18987))
      }else {
        if("\ufdd0'else") {
          return inode__18985
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_assoc = function(shift, hash, key, val, added_leaf_QMARK_) {
  var this__18991 = this;
  var inode__18992 = this;
  var bit__18993 = 1 << (hash >>> shift & 31);
  var idx__18994 = cljs.core.bitmap_indexed_node_index.call(null, this__18991.bitmap, bit__18993);
  if((this__18991.bitmap & bit__18993) === 0) {
    var n__18995 = cljs.core.bit_count.call(null, this__18991.bitmap);
    if(n__18995 >= 16) {
      var nodes__18996 = cljs.core.make_array.call(null, 32);
      var jdx__18997 = hash >>> shift & 31;
      nodes__18996[jdx__18997] = cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_);
      var i__18998 = 0;
      var j__18999 = 0;
      while(true) {
        if(i__18998 < 32) {
          if((this__18991.bitmap >>> i__18998 & 1) === 0) {
            var G__19014 = i__18998 + 1;
            var G__19015 = j__18999;
            i__18998 = G__19014;
            j__18999 = G__19015;
            continue
          }else {
            nodes__18996[i__18998] = !(this__18991.arr[j__18999] == null) ? cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift + 5, cljs.core.hash.call(null, this__18991.arr[j__18999]), this__18991.arr[j__18999], this__18991.arr[j__18999 + 1], added_leaf_QMARK_) : this__18991.arr[j__18999 + 1];
            var G__19016 = i__18998 + 1;
            var G__19017 = j__18999 + 2;
            i__18998 = G__19016;
            j__18999 = G__19017;
            continue
          }
        }else {
        }
        break
      }
      return new cljs.core.ArrayNode(null, n__18995 + 1, nodes__18996)
    }else {
      var new_arr__19000 = cljs.core.make_array.call(null, 2 * (n__18995 + 1));
      cljs.core.array_copy.call(null, this__18991.arr, 0, new_arr__19000, 0, 2 * idx__18994);
      new_arr__19000[2 * idx__18994] = key;
      new_arr__19000[2 * idx__18994 + 1] = val;
      cljs.core.array_copy.call(null, this__18991.arr, 2 * idx__18994, new_arr__19000, 2 * (idx__18994 + 1), 2 * (n__18995 - idx__18994));
      added_leaf_QMARK_.val = true;
      return new cljs.core.BitmapIndexedNode(null, this__18991.bitmap | bit__18993, new_arr__19000)
    }
  }else {
    var key_or_nil__19001 = this__18991.arr[2 * idx__18994];
    var val_or_node__19002 = this__18991.arr[2 * idx__18994 + 1];
    if(key_or_nil__19001 == null) {
      var n__19003 = val_or_node__19002.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_);
      if(n__19003 === val_or_node__19002) {
        return inode__18992
      }else {
        return new cljs.core.BitmapIndexedNode(null, this__18991.bitmap, cljs.core.clone_and_set.call(null, this__18991.arr, 2 * idx__18994 + 1, n__19003))
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19001)) {
        if(val === val_or_node__19002) {
          return inode__18992
        }else {
          return new cljs.core.BitmapIndexedNode(null, this__18991.bitmap, cljs.core.clone_and_set.call(null, this__18991.arr, 2 * idx__18994 + 1, val))
        }
      }else {
        if("\ufdd0'else") {
          added_leaf_QMARK_.val = true;
          return new cljs.core.BitmapIndexedNode(null, this__18991.bitmap, cljs.core.clone_and_set.call(null, this__18991.arr, 2 * idx__18994, null, 2 * idx__18994 + 1, cljs.core.create_node.call(null, shift + 5, key_or_nil__19001, val_or_node__19002, hash, key, val)))
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_lookup = function(shift, hash, key, not_found) {
  var this__19004 = this;
  var inode__19005 = this;
  var bit__19006 = 1 << (hash >>> shift & 31);
  if((this__19004.bitmap & bit__19006) === 0) {
    return not_found
  }else {
    var idx__19007 = cljs.core.bitmap_indexed_node_index.call(null, this__19004.bitmap, bit__19006);
    var key_or_nil__19008 = this__19004.arr[2 * idx__19007];
    var val_or_node__19009 = this__19004.arr[2 * idx__19007 + 1];
    if(key_or_nil__19008 == null) {
      return val_or_node__19009.inode_lookup(shift + 5, hash, key, not_found)
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19008)) {
        return val_or_node__19009
      }else {
        if("\ufdd0'else") {
          return not_found
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode;
cljs.core.BitmapIndexedNode.EMPTY = new cljs.core.BitmapIndexedNode(null, 0, cljs.core.make_array.call(null, 0));
cljs.core.pack_array_node = function pack_array_node(array_node, edit, idx) {
  var arr__19025 = array_node.arr;
  var len__19026 = 2 * (array_node.cnt - 1);
  var new_arr__19027 = cljs.core.make_array.call(null, len__19026);
  var i__19028 = 0;
  var j__19029 = 1;
  var bitmap__19030 = 0;
  while(true) {
    if(i__19028 < len__19026) {
      if(function() {
        var and__3822__auto____19031 = !(i__19028 === idx);
        if(and__3822__auto____19031) {
          return!(arr__19025[i__19028] == null)
        }else {
          return and__3822__auto____19031
        }
      }()) {
        new_arr__19027[j__19029] = arr__19025[i__19028];
        var G__19032 = i__19028 + 1;
        var G__19033 = j__19029 + 2;
        var G__19034 = bitmap__19030 | 1 << i__19028;
        i__19028 = G__19032;
        j__19029 = G__19033;
        bitmap__19030 = G__19034;
        continue
      }else {
        var G__19035 = i__19028 + 1;
        var G__19036 = j__19029;
        var G__19037 = bitmap__19030;
        i__19028 = G__19035;
        j__19029 = G__19036;
        bitmap__19030 = G__19037;
        continue
      }
    }else {
      return new cljs.core.BitmapIndexedNode(edit, bitmap__19030, new_arr__19027)
    }
    break
  }
};
goog.provide("cljs.core.ArrayNode");
cljs.core.ArrayNode = function(edit, cnt, arr) {
  this.edit = edit;
  this.cnt = cnt;
  this.arr = arr
};
cljs.core.ArrayNode.cljs$lang$type = true;
cljs.core.ArrayNode.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ArrayNode")
};
cljs.core.ArrayNode.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ArrayNode")
};
cljs.core.ArrayNode.prototype.inode_assoc_BANG_ = function(edit, shift, hash, key, val, added_leaf_QMARK_) {
  var this__19038 = this;
  var inode__19039 = this;
  var idx__19040 = hash >>> shift & 31;
  var node__19041 = this__19038.arr[idx__19040];
  if(node__19041 == null) {
    var editable__19042 = cljs.core.edit_and_set.call(null, inode__19039, edit, idx__19040, cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_));
    editable__19042.cnt = editable__19042.cnt + 1;
    return editable__19042
  }else {
    var n__19043 = node__19041.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_);
    if(n__19043 === node__19041) {
      return inode__19039
    }else {
      return cljs.core.edit_and_set.call(null, inode__19039, edit, idx__19040, n__19043)
    }
  }
};
cljs.core.ArrayNode.prototype.inode_seq = function() {
  var this__19044 = this;
  var inode__19045 = this;
  return cljs.core.create_array_node_seq.call(null, this__19044.arr)
};
cljs.core.ArrayNode.prototype.inode_without_BANG_ = function(edit, shift, hash, key, removed_leaf_QMARK_) {
  var this__19046 = this;
  var inode__19047 = this;
  var idx__19048 = hash >>> shift & 31;
  var node__19049 = this__19046.arr[idx__19048];
  if(node__19049 == null) {
    return inode__19047
  }else {
    var n__19050 = node__19049.inode_without_BANG_(edit, shift + 5, hash, key, removed_leaf_QMARK_);
    if(n__19050 === node__19049) {
      return inode__19047
    }else {
      if(n__19050 == null) {
        if(this__19046.cnt <= 8) {
          return cljs.core.pack_array_node.call(null, inode__19047, edit, idx__19048)
        }else {
          var editable__19051 = cljs.core.edit_and_set.call(null, inode__19047, edit, idx__19048, n__19050);
          editable__19051.cnt = editable__19051.cnt - 1;
          return editable__19051
        }
      }else {
        if("\ufdd0'else") {
          return cljs.core.edit_and_set.call(null, inode__19047, edit, idx__19048, n__19050)
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.ArrayNode.prototype.ensure_editable = function(e) {
  var this__19052 = this;
  var inode__19053 = this;
  if(e === this__19052.edit) {
    return inode__19053
  }else {
    return new cljs.core.ArrayNode(e, this__19052.cnt, this__19052.arr.slice())
  }
};
cljs.core.ArrayNode.prototype.kv_reduce = function(f, init) {
  var this__19054 = this;
  var inode__19055 = this;
  var len__19056 = this__19054.arr.length;
  var i__19057 = 0;
  var init__19058 = init;
  while(true) {
    if(i__19057 < len__19056) {
      var node__19059 = this__19054.arr[i__19057];
      if(!(node__19059 == null)) {
        var init__19060 = node__19059.kv_reduce(f, init__19058);
        if(cljs.core.reduced_QMARK_.call(null, init__19060)) {
          return cljs.core.deref.call(null, init__19060)
        }else {
          var G__19079 = i__19057 + 1;
          var G__19080 = init__19060;
          i__19057 = G__19079;
          init__19058 = G__19080;
          continue
        }
      }else {
        return null
      }
    }else {
      return init__19058
    }
    break
  }
};
cljs.core.ArrayNode.prototype.inode_find = function(shift, hash, key, not_found) {
  var this__19061 = this;
  var inode__19062 = this;
  var idx__19063 = hash >>> shift & 31;
  var node__19064 = this__19061.arr[idx__19063];
  if(!(node__19064 == null)) {
    return node__19064.inode_find(shift + 5, hash, key, not_found)
  }else {
    return not_found
  }
};
cljs.core.ArrayNode.prototype.inode_without = function(shift, hash, key) {
  var this__19065 = this;
  var inode__19066 = this;
  var idx__19067 = hash >>> shift & 31;
  var node__19068 = this__19065.arr[idx__19067];
  if(!(node__19068 == null)) {
    var n__19069 = node__19068.inode_without(shift + 5, hash, key);
    if(n__19069 === node__19068) {
      return inode__19066
    }else {
      if(n__19069 == null) {
        if(this__19065.cnt <= 8) {
          return cljs.core.pack_array_node.call(null, inode__19066, null, idx__19067)
        }else {
          return new cljs.core.ArrayNode(null, this__19065.cnt - 1, cljs.core.clone_and_set.call(null, this__19065.arr, idx__19067, n__19069))
        }
      }else {
        if("\ufdd0'else") {
          return new cljs.core.ArrayNode(null, this__19065.cnt, cljs.core.clone_and_set.call(null, this__19065.arr, idx__19067, n__19069))
        }else {
          return null
        }
      }
    }
  }else {
    return inode__19066
  }
};
cljs.core.ArrayNode.prototype.inode_assoc = function(shift, hash, key, val, added_leaf_QMARK_) {
  var this__19070 = this;
  var inode__19071 = this;
  var idx__19072 = hash >>> shift & 31;
  var node__19073 = this__19070.arr[idx__19072];
  if(node__19073 == null) {
    return new cljs.core.ArrayNode(null, this__19070.cnt + 1, cljs.core.clone_and_set.call(null, this__19070.arr, idx__19072, cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_)))
  }else {
    var n__19074 = node__19073.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_);
    if(n__19074 === node__19073) {
      return inode__19071
    }else {
      return new cljs.core.ArrayNode(null, this__19070.cnt, cljs.core.clone_and_set.call(null, this__19070.arr, idx__19072, n__19074))
    }
  }
};
cljs.core.ArrayNode.prototype.inode_lookup = function(shift, hash, key, not_found) {
  var this__19075 = this;
  var inode__19076 = this;
  var idx__19077 = hash >>> shift & 31;
  var node__19078 = this__19075.arr[idx__19077];
  if(!(node__19078 == null)) {
    return node__19078.inode_lookup(shift + 5, hash, key, not_found)
  }else {
    return not_found
  }
};
cljs.core.ArrayNode;
cljs.core.hash_collision_node_find_index = function hash_collision_node_find_index(arr, cnt, key) {
  var lim__19083 = 2 * cnt;
  var i__19084 = 0;
  while(true) {
    if(i__19084 < lim__19083) {
      if(cljs.core.key_test.call(null, key, arr[i__19084])) {
        return i__19084
      }else {
        var G__19085 = i__19084 + 2;
        i__19084 = G__19085;
        continue
      }
    }else {
      return-1
    }
    break
  }
};
goog.provide("cljs.core.HashCollisionNode");
cljs.core.HashCollisionNode = function(edit, collision_hash, cnt, arr) {
  this.edit = edit;
  this.collision_hash = collision_hash;
  this.cnt = cnt;
  this.arr = arr
};
cljs.core.HashCollisionNode.cljs$lang$type = true;
cljs.core.HashCollisionNode.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/HashCollisionNode")
};
cljs.core.HashCollisionNode.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/HashCollisionNode")
};
cljs.core.HashCollisionNode.prototype.inode_assoc_BANG_ = function(edit, shift, hash, key, val, added_leaf_QMARK_) {
  var this__19086 = this;
  var inode__19087 = this;
  if(hash === this__19086.collision_hash) {
    var idx__19088 = cljs.core.hash_collision_node_find_index.call(null, this__19086.arr, this__19086.cnt, key);
    if(idx__19088 === -1) {
      if(this__19086.arr.length > 2 * this__19086.cnt) {
        var editable__19089 = cljs.core.edit_and_set.call(null, inode__19087, edit, 2 * this__19086.cnt, key, 2 * this__19086.cnt + 1, val);
        added_leaf_QMARK_.val = true;
        editable__19089.cnt = editable__19089.cnt + 1;
        return editable__19089
      }else {
        var len__19090 = this__19086.arr.length;
        var new_arr__19091 = cljs.core.make_array.call(null, len__19090 + 2);
        cljs.core.array_copy.call(null, this__19086.arr, 0, new_arr__19091, 0, len__19090);
        new_arr__19091[len__19090] = key;
        new_arr__19091[len__19090 + 1] = val;
        added_leaf_QMARK_.val = true;
        return inode__19087.ensure_editable_array(edit, this__19086.cnt + 1, new_arr__19091)
      }
    }else {
      if(this__19086.arr[idx__19088 + 1] === val) {
        return inode__19087
      }else {
        return cljs.core.edit_and_set.call(null, inode__19087, edit, idx__19088 + 1, val)
      }
    }
  }else {
    return(new cljs.core.BitmapIndexedNode(edit, 1 << (this__19086.collision_hash >>> shift & 31), [null, inode__19087, null, null])).inode_assoc_BANG_(edit, shift, hash, key, val, added_leaf_QMARK_)
  }
};
cljs.core.HashCollisionNode.prototype.inode_seq = function() {
  var this__19092 = this;
  var inode__19093 = this;
  return cljs.core.create_inode_seq.call(null, this__19092.arr)
};
cljs.core.HashCollisionNode.prototype.inode_without_BANG_ = function(edit, shift, hash, key, removed_leaf_QMARK_) {
  var this__19094 = this;
  var inode__19095 = this;
  var idx__19096 = cljs.core.hash_collision_node_find_index.call(null, this__19094.arr, this__19094.cnt, key);
  if(idx__19096 === -1) {
    return inode__19095
  }else {
    removed_leaf_QMARK_[0] = true;
    if(this__19094.cnt === 1) {
      return null
    }else {
      var editable__19097 = inode__19095.ensure_editable(edit);
      var earr__19098 = editable__19097.arr;
      earr__19098[idx__19096] = earr__19098[2 * this__19094.cnt - 2];
      earr__19098[idx__19096 + 1] = earr__19098[2 * this__19094.cnt - 1];
      earr__19098[2 * this__19094.cnt - 1] = null;
      earr__19098[2 * this__19094.cnt - 2] = null;
      editable__19097.cnt = editable__19097.cnt - 1;
      return editable__19097
    }
  }
};
cljs.core.HashCollisionNode.prototype.ensure_editable = function(e) {
  var this__19099 = this;
  var inode__19100 = this;
  if(e === this__19099.edit) {
    return inode__19100
  }else {
    var new_arr__19101 = cljs.core.make_array.call(null, 2 * (this__19099.cnt + 1));
    cljs.core.array_copy.call(null, this__19099.arr, 0, new_arr__19101, 0, 2 * this__19099.cnt);
    return new cljs.core.HashCollisionNode(e, this__19099.collision_hash, this__19099.cnt, new_arr__19101)
  }
};
cljs.core.HashCollisionNode.prototype.kv_reduce = function(f, init) {
  var this__19102 = this;
  var inode__19103 = this;
  return cljs.core.inode_kv_reduce.call(null, this__19102.arr, f, init)
};
cljs.core.HashCollisionNode.prototype.inode_find = function(shift, hash, key, not_found) {
  var this__19104 = this;
  var inode__19105 = this;
  var idx__19106 = cljs.core.hash_collision_node_find_index.call(null, this__19104.arr, this__19104.cnt, key);
  if(idx__19106 < 0) {
    return not_found
  }else {
    if(cljs.core.key_test.call(null, key, this__19104.arr[idx__19106])) {
      return cljs.core.PersistentVector.fromArray([this__19104.arr[idx__19106], this__19104.arr[idx__19106 + 1]], true)
    }else {
      if("\ufdd0'else") {
        return not_found
      }else {
        return null
      }
    }
  }
};
cljs.core.HashCollisionNode.prototype.inode_without = function(shift, hash, key) {
  var this__19107 = this;
  var inode__19108 = this;
  var idx__19109 = cljs.core.hash_collision_node_find_index.call(null, this__19107.arr, this__19107.cnt, key);
  if(idx__19109 === -1) {
    return inode__19108
  }else {
    if(this__19107.cnt === 1) {
      return null
    }else {
      if("\ufdd0'else") {
        return new cljs.core.HashCollisionNode(null, this__19107.collision_hash, this__19107.cnt - 1, cljs.core.remove_pair.call(null, this__19107.arr, cljs.core.quot.call(null, idx__19109, 2)))
      }else {
        return null
      }
    }
  }
};
cljs.core.HashCollisionNode.prototype.inode_assoc = function(shift, hash, key, val, added_leaf_QMARK_) {
  var this__19110 = this;
  var inode__19111 = this;
  if(hash === this__19110.collision_hash) {
    var idx__19112 = cljs.core.hash_collision_node_find_index.call(null, this__19110.arr, this__19110.cnt, key);
    if(idx__19112 === -1) {
      var len__19113 = this__19110.arr.length;
      var new_arr__19114 = cljs.core.make_array.call(null, len__19113 + 2);
      cljs.core.array_copy.call(null, this__19110.arr, 0, new_arr__19114, 0, len__19113);
      new_arr__19114[len__19113] = key;
      new_arr__19114[len__19113 + 1] = val;
      added_leaf_QMARK_.val = true;
      return new cljs.core.HashCollisionNode(null, this__19110.collision_hash, this__19110.cnt + 1, new_arr__19114)
    }else {
      if(cljs.core._EQ_.call(null, this__19110.arr[idx__19112], val)) {
        return inode__19111
      }else {
        return new cljs.core.HashCollisionNode(null, this__19110.collision_hash, this__19110.cnt, cljs.core.clone_and_set.call(null, this__19110.arr, idx__19112 + 1, val))
      }
    }
  }else {
    return(new cljs.core.BitmapIndexedNode(null, 1 << (this__19110.collision_hash >>> shift & 31), [null, inode__19111])).inode_assoc(shift, hash, key, val, added_leaf_QMARK_)
  }
};
cljs.core.HashCollisionNode.prototype.inode_lookup = function(shift, hash, key, not_found) {
  var this__19115 = this;
  var inode__19116 = this;
  var idx__19117 = cljs.core.hash_collision_node_find_index.call(null, this__19115.arr, this__19115.cnt, key);
  if(idx__19117 < 0) {
    return not_found
  }else {
    if(cljs.core.key_test.call(null, key, this__19115.arr[idx__19117])) {
      return this__19115.arr[idx__19117 + 1]
    }else {
      if("\ufdd0'else") {
        return not_found
      }else {
        return null
      }
    }
  }
};
cljs.core.HashCollisionNode.prototype.ensure_editable_array = function(e, count, array) {
  var this__19118 = this;
  var inode__19119 = this;
  if(e === this__19118.edit) {
    this__19118.arr = array;
    this__19118.cnt = count;
    return inode__19119
  }else {
    return new cljs.core.HashCollisionNode(this__19118.edit, this__19118.collision_hash, count, array)
  }
};
cljs.core.HashCollisionNode;
cljs.core.create_node = function() {
  var create_node = null;
  var create_node__6 = function(shift, key1, val1, key2hash, key2, val2) {
    var key1hash__19124 = cljs.core.hash.call(null, key1);
    if(key1hash__19124 === key2hash) {
      return new cljs.core.HashCollisionNode(null, key1hash__19124, 2, [key1, val1, key2, val2])
    }else {
      var added_leaf_QMARK___19125 = new cljs.core.Box(false);
      return cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift, key1hash__19124, key1, val1, added_leaf_QMARK___19125).inode_assoc(shift, key2hash, key2, val2, added_leaf_QMARK___19125)
    }
  };
  var create_node__7 = function(edit, shift, key1, val1, key2hash, key2, val2) {
    var key1hash__19126 = cljs.core.hash.call(null, key1);
    if(key1hash__19126 === key2hash) {
      return new cljs.core.HashCollisionNode(null, key1hash__19126, 2, [key1, val1, key2, val2])
    }else {
      var added_leaf_QMARK___19127 = new cljs.core.Box(false);
      return cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift, key1hash__19126, key1, val1, added_leaf_QMARK___19127).inode_assoc_BANG_(edit, shift, key2hash, key2, val2, added_leaf_QMARK___19127)
    }
  };
  create_node = function(edit, shift, key1, val1, key2hash, key2, val2) {
    switch(arguments.length) {
      case 6:
        return create_node__6.call(this, edit, shift, key1, val1, key2hash, key2);
      case 7:
        return create_node__7.call(this, edit, shift, key1, val1, key2hash, key2, val2)
    }
    throw"Invalid arity: " + arguments.length;
  };
  create_node.cljs$lang$arity$6 = create_node__6;
  create_node.cljs$lang$arity$7 = create_node__7;
  return create_node
}();
goog.provide("cljs.core.NodeSeq");
cljs.core.NodeSeq = function(meta, nodes, i, s, __hash) {
  this.meta = meta;
  this.nodes = nodes;
  this.i = i;
  this.s = s;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850572
};
cljs.core.NodeSeq.cljs$lang$type = true;
cljs.core.NodeSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/NodeSeq")
};
cljs.core.NodeSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/NodeSeq")
};
cljs.core.NodeSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19128 = this;
  var h__2247__auto____19129 = this__19128.__hash;
  if(!(h__2247__auto____19129 == null)) {
    return h__2247__auto____19129
  }else {
    var h__2247__auto____19130 = cljs.core.hash_coll.call(null, coll);
    this__19128.__hash = h__2247__auto____19130;
    return h__2247__auto____19130
  }
};
cljs.core.NodeSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19131 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.NodeSeq.prototype.toString = function() {
  var this__19132 = this;
  var this__19133 = this;
  return cljs.core.pr_str.call(null, this__19133)
};
cljs.core.NodeSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__19134 = this;
  return this$
};
cljs.core.NodeSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__19135 = this;
  if(this__19135.s == null) {
    return cljs.core.PersistentVector.fromArray([this__19135.nodes[this__19135.i], this__19135.nodes[this__19135.i + 1]], true)
  }else {
    return cljs.core.first.call(null, this__19135.s)
  }
};
cljs.core.NodeSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__19136 = this;
  if(this__19136.s == null) {
    return cljs.core.create_inode_seq.call(null, this__19136.nodes, this__19136.i + 2, null)
  }else {
    return cljs.core.create_inode_seq.call(null, this__19136.nodes, this__19136.i, cljs.core.next.call(null, this__19136.s))
  }
};
cljs.core.NodeSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19137 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.NodeSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19138 = this;
  return new cljs.core.NodeSeq(meta, this__19138.nodes, this__19138.i, this__19138.s, this__19138.__hash)
};
cljs.core.NodeSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19139 = this;
  return this__19139.meta
};
cljs.core.NodeSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19140 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19140.meta)
};
cljs.core.NodeSeq;
cljs.core.create_inode_seq = function() {
  var create_inode_seq = null;
  var create_inode_seq__1 = function(nodes) {
    return create_inode_seq.call(null, nodes, 0, null)
  };
  var create_inode_seq__3 = function(nodes, i, s) {
    if(s == null) {
      var len__19147 = nodes.length;
      var j__19148 = i;
      while(true) {
        if(j__19148 < len__19147) {
          if(!(nodes[j__19148] == null)) {
            return new cljs.core.NodeSeq(null, nodes, j__19148, null, null)
          }else {
            var temp__3971__auto____19149 = nodes[j__19148 + 1];
            if(cljs.core.truth_(temp__3971__auto____19149)) {
              var node__19150 = temp__3971__auto____19149;
              var temp__3971__auto____19151 = node__19150.inode_seq();
              if(cljs.core.truth_(temp__3971__auto____19151)) {
                var node_seq__19152 = temp__3971__auto____19151;
                return new cljs.core.NodeSeq(null, nodes, j__19148 + 2, node_seq__19152, null)
              }else {
                var G__19153 = j__19148 + 2;
                j__19148 = G__19153;
                continue
              }
            }else {
              var G__19154 = j__19148 + 2;
              j__19148 = G__19154;
              continue
            }
          }
        }else {
          return null
        }
        break
      }
    }else {
      return new cljs.core.NodeSeq(null, nodes, i, s, null)
    }
  };
  create_inode_seq = function(nodes, i, s) {
    switch(arguments.length) {
      case 1:
        return create_inode_seq__1.call(this, nodes);
      case 3:
        return create_inode_seq__3.call(this, nodes, i, s)
    }
    throw"Invalid arity: " + arguments.length;
  };
  create_inode_seq.cljs$lang$arity$1 = create_inode_seq__1;
  create_inode_seq.cljs$lang$arity$3 = create_inode_seq__3;
  return create_inode_seq
}();
goog.provide("cljs.core.ArrayNodeSeq");
cljs.core.ArrayNodeSeq = function(meta, nodes, i, s, __hash) {
  this.meta = meta;
  this.nodes = nodes;
  this.i = i;
  this.s = s;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850572
};
cljs.core.ArrayNodeSeq.cljs$lang$type = true;
cljs.core.ArrayNodeSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/ArrayNodeSeq")
};
cljs.core.ArrayNodeSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/ArrayNodeSeq")
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19155 = this;
  var h__2247__auto____19156 = this__19155.__hash;
  if(!(h__2247__auto____19156 == null)) {
    return h__2247__auto____19156
  }else {
    var h__2247__auto____19157 = cljs.core.hash_coll.call(null, coll);
    this__19155.__hash = h__2247__auto____19157;
    return h__2247__auto____19157
  }
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19158 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.ArrayNodeSeq.prototype.toString = function() {
  var this__19159 = this;
  var this__19160 = this;
  return cljs.core.pr_str.call(null, this__19160)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__19161 = this;
  return this$
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__19162 = this;
  return cljs.core.first.call(null, this__19162.s)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__19163 = this;
  return cljs.core.create_array_node_seq.call(null, null, this__19163.nodes, this__19163.i, cljs.core.next.call(null, this__19163.s))
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19164 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19165 = this;
  return new cljs.core.ArrayNodeSeq(meta, this__19165.nodes, this__19165.i, this__19165.s, this__19165.__hash)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19166 = this;
  return this__19166.meta
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19167 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19167.meta)
};
cljs.core.ArrayNodeSeq;
cljs.core.create_array_node_seq = function() {
  var create_array_node_seq = null;
  var create_array_node_seq__1 = function(nodes) {
    return create_array_node_seq.call(null, null, nodes, 0, null)
  };
  var create_array_node_seq__4 = function(meta, nodes, i, s) {
    if(s == null) {
      var len__19174 = nodes.length;
      var j__19175 = i;
      while(true) {
        if(j__19175 < len__19174) {
          var temp__3971__auto____19176 = nodes[j__19175];
          if(cljs.core.truth_(temp__3971__auto____19176)) {
            var nj__19177 = temp__3971__auto____19176;
            var temp__3971__auto____19178 = nj__19177.inode_seq();
            if(cljs.core.truth_(temp__3971__auto____19178)) {
              var ns__19179 = temp__3971__auto____19178;
              return new cljs.core.ArrayNodeSeq(meta, nodes, j__19175 + 1, ns__19179, null)
            }else {
              var G__19180 = j__19175 + 1;
              j__19175 = G__19180;
              continue
            }
          }else {
            var G__19181 = j__19175 + 1;
            j__19175 = G__19181;
            continue
          }
        }else {
          return null
        }
        break
      }
    }else {
      return new cljs.core.ArrayNodeSeq(meta, nodes, i, s, null)
    }
  };
  create_array_node_seq = function(meta, nodes, i, s) {
    switch(arguments.length) {
      case 1:
        return create_array_node_seq__1.call(this, meta);
      case 4:
        return create_array_node_seq__4.call(this, meta, nodes, i, s)
    }
    throw"Invalid arity: " + arguments.length;
  };
  create_array_node_seq.cljs$lang$arity$1 = create_array_node_seq__1;
  create_array_node_seq.cljs$lang$arity$4 = create_array_node_seq__4;
  return create_array_node_seq
}();
goog.provide("cljs.core.PersistentHashMap");
cljs.core.PersistentHashMap = function(meta, cnt, root, has_nil_QMARK_, nil_val, __hash) {
  this.meta = meta;
  this.cnt = cnt;
  this.root = root;
  this.has_nil_QMARK_ = has_nil_QMARK_;
  this.nil_val = nil_val;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 4;
  this.cljs$lang$protocol_mask$partition0$ = 16123663
};
cljs.core.PersistentHashMap.cljs$lang$type = true;
cljs.core.PersistentHashMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentHashMap")
};
cljs.core.PersistentHashMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentHashMap")
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(coll) {
  var this__19184 = this;
  return new cljs.core.TransientHashMap({}, this__19184.root, this__19184.cnt, this__19184.has_nil_QMARK_, this__19184.nil_val)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19185 = this;
  var h__2247__auto____19186 = this__19185.__hash;
  if(!(h__2247__auto____19186 == null)) {
    return h__2247__auto____19186
  }else {
    var h__2247__auto____19187 = cljs.core.hash_imap.call(null, coll);
    this__19185.__hash = h__2247__auto____19187;
    return h__2247__auto____19187
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19188 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.PersistentHashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19189 = this;
  if(k == null) {
    if(this__19189.has_nil_QMARK_) {
      return this__19189.nil_val
    }else {
      return not_found
    }
  }else {
    if(this__19189.root == null) {
      return not_found
    }else {
      if("\ufdd0'else") {
        return this__19189.root.inode_lookup(0, cljs.core.hash.call(null, k), k, not_found)
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19190 = this;
  if(k == null) {
    if(function() {
      var and__3822__auto____19191 = this__19190.has_nil_QMARK_;
      if(and__3822__auto____19191) {
        return v === this__19190.nil_val
      }else {
        return and__3822__auto____19191
      }
    }()) {
      return coll
    }else {
      return new cljs.core.PersistentHashMap(this__19190.meta, this__19190.has_nil_QMARK_ ? this__19190.cnt : this__19190.cnt + 1, this__19190.root, true, v, null)
    }
  }else {
    var added_leaf_QMARK___19192 = new cljs.core.Box(false);
    var new_root__19193 = (this__19190.root == null ? cljs.core.BitmapIndexedNode.EMPTY : this__19190.root).inode_assoc(0, cljs.core.hash.call(null, k), k, v, added_leaf_QMARK___19192);
    if(new_root__19193 === this__19190.root) {
      return coll
    }else {
      return new cljs.core.PersistentHashMap(this__19190.meta, added_leaf_QMARK___19192.val ? this__19190.cnt + 1 : this__19190.cnt, new_root__19193, this__19190.has_nil_QMARK_, this__19190.nil_val, null)
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19194 = this;
  if(k == null) {
    return this__19194.has_nil_QMARK_
  }else {
    if(this__19194.root == null) {
      return false
    }else {
      if("\ufdd0'else") {
        return!(this__19194.root.inode_lookup(0, cljs.core.hash.call(null, k), k, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel)
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.call = function() {
  var G__19217 = null;
  var G__19217__2 = function(this_sym19195, k) {
    var this__19197 = this;
    var this_sym19195__19198 = this;
    var coll__19199 = this_sym19195__19198;
    return coll__19199.cljs$core$ILookup$_lookup$arity$2(coll__19199, k)
  };
  var G__19217__3 = function(this_sym19196, k, not_found) {
    var this__19197 = this;
    var this_sym19196__19200 = this;
    var coll__19201 = this_sym19196__19200;
    return coll__19201.cljs$core$ILookup$_lookup$arity$3(coll__19201, k, not_found)
  };
  G__19217 = function(this_sym19196, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19217__2.call(this, this_sym19196, k);
      case 3:
        return G__19217__3.call(this, this_sym19196, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19217
}();
cljs.core.PersistentHashMap.prototype.apply = function(this_sym19182, args19183) {
  var this__19202 = this;
  return this_sym19182.call.apply(this_sym19182, [this_sym19182].concat(args19183.slice()))
};
cljs.core.PersistentHashMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(coll, f, init) {
  var this__19203 = this;
  var init__19204 = this__19203.has_nil_QMARK_ ? f.call(null, init, null, this__19203.nil_val) : init;
  if(cljs.core.reduced_QMARK_.call(null, init__19204)) {
    return cljs.core.deref.call(null, init__19204)
  }else {
    if(!(this__19203.root == null)) {
      return this__19203.root.kv_reduce(f, init__19204)
    }else {
      if("\ufdd0'else") {
        return init__19204
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19205 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.PersistentHashMap.prototype.toString = function() {
  var this__19206 = this;
  var this__19207 = this;
  return cljs.core.pr_str.call(null, this__19207)
};
cljs.core.PersistentHashMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19208 = this;
  if(this__19208.cnt > 0) {
    var s__19209 = !(this__19208.root == null) ? this__19208.root.inode_seq() : null;
    if(this__19208.has_nil_QMARK_) {
      return cljs.core.cons.call(null, cljs.core.PersistentVector.fromArray([null, this__19208.nil_val], true), s__19209)
    }else {
      return s__19209
    }
  }else {
    return null
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19210 = this;
  return this__19210.cnt
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19211 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19212 = this;
  return new cljs.core.PersistentHashMap(meta, this__19212.cnt, this__19212.root, this__19212.has_nil_QMARK_, this__19212.nil_val, this__19212.__hash)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19213 = this;
  return this__19213.meta
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19214 = this;
  return cljs.core._with_meta.call(null, cljs.core.PersistentHashMap.EMPTY, this__19214.meta)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19215 = this;
  if(k == null) {
    if(this__19215.has_nil_QMARK_) {
      return new cljs.core.PersistentHashMap(this__19215.meta, this__19215.cnt - 1, this__19215.root, false, null, null)
    }else {
      return coll
    }
  }else {
    if(this__19215.root == null) {
      return coll
    }else {
      if("\ufdd0'else") {
        var new_root__19216 = this__19215.root.inode_without(0, cljs.core.hash.call(null, k), k);
        if(new_root__19216 === this__19215.root) {
          return coll
        }else {
          return new cljs.core.PersistentHashMap(this__19215.meta, this__19215.cnt - 1, new_root__19216, this__19215.has_nil_QMARK_, this__19215.nil_val, null)
        }
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap;
cljs.core.PersistentHashMap.EMPTY = new cljs.core.PersistentHashMap(null, 0, null, false, null, 0);
cljs.core.PersistentHashMap.fromArrays = function(ks, vs) {
  var len__19218 = ks.length;
  var i__19219 = 0;
  var out__19220 = cljs.core.transient$.call(null, cljs.core.PersistentHashMap.EMPTY);
  while(true) {
    if(i__19219 < len__19218) {
      var G__19221 = i__19219 + 1;
      var G__19222 = cljs.core.assoc_BANG_.call(null, out__19220, ks[i__19219], vs[i__19219]);
      i__19219 = G__19221;
      out__19220 = G__19222;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, out__19220)
    }
    break
  }
};
goog.provide("cljs.core.TransientHashMap");
cljs.core.TransientHashMap = function(edit, root, count, has_nil_QMARK_, nil_val) {
  this.edit = edit;
  this.root = root;
  this.count = count;
  this.has_nil_QMARK_ = has_nil_QMARK_;
  this.nil_val = nil_val;
  this.cljs$lang$protocol_mask$partition1$ = 56;
  this.cljs$lang$protocol_mask$partition0$ = 258
};
cljs.core.TransientHashMap.cljs$lang$type = true;
cljs.core.TransientHashMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/TransientHashMap")
};
cljs.core.TransientHashMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/TransientHashMap")
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientMap$_dissoc_BANG_$arity$2 = function(tcoll, key) {
  var this__19223 = this;
  return tcoll.without_BANG_(key)
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(tcoll, key, val) {
  var this__19224 = this;
  return tcoll.assoc_BANG_(key, val)
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, val) {
  var this__19225 = this;
  return tcoll.conj_BANG_(val)
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__19226 = this;
  return tcoll.persistent_BANG_()
};
cljs.core.TransientHashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(tcoll, k) {
  var this__19227 = this;
  if(k == null) {
    if(this__19227.has_nil_QMARK_) {
      return this__19227.nil_val
    }else {
      return null
    }
  }else {
    if(this__19227.root == null) {
      return null
    }else {
      return this__19227.root.inode_lookup(0, cljs.core.hash.call(null, k), k)
    }
  }
};
cljs.core.TransientHashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(tcoll, k, not_found) {
  var this__19228 = this;
  if(k == null) {
    if(this__19228.has_nil_QMARK_) {
      return this__19228.nil_val
    }else {
      return not_found
    }
  }else {
    if(this__19228.root == null) {
      return not_found
    }else {
      return this__19228.root.inode_lookup(0, cljs.core.hash.call(null, k), k, not_found)
    }
  }
};
cljs.core.TransientHashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19229 = this;
  if(this__19229.edit) {
    return this__19229.count
  }else {
    throw new Error("count after persistent!");
  }
};
cljs.core.TransientHashMap.prototype.conj_BANG_ = function(o) {
  var this__19230 = this;
  var tcoll__19231 = this;
  if(this__19230.edit) {
    if(function() {
      var G__19232__19233 = o;
      if(G__19232__19233) {
        if(function() {
          var or__3824__auto____19234 = G__19232__19233.cljs$lang$protocol_mask$partition0$ & 2048;
          if(or__3824__auto____19234) {
            return or__3824__auto____19234
          }else {
            return G__19232__19233.cljs$core$IMapEntry$
          }
        }()) {
          return true
        }else {
          if(!G__19232__19233.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__19232__19233)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__19232__19233)
      }
    }()) {
      return tcoll__19231.assoc_BANG_(cljs.core.key.call(null, o), cljs.core.val.call(null, o))
    }else {
      var es__19235 = cljs.core.seq.call(null, o);
      var tcoll__19236 = tcoll__19231;
      while(true) {
        var temp__3971__auto____19237 = cljs.core.first.call(null, es__19235);
        if(cljs.core.truth_(temp__3971__auto____19237)) {
          var e__19238 = temp__3971__auto____19237;
          var G__19249 = cljs.core.next.call(null, es__19235);
          var G__19250 = tcoll__19236.assoc_BANG_(cljs.core.key.call(null, e__19238), cljs.core.val.call(null, e__19238));
          es__19235 = G__19249;
          tcoll__19236 = G__19250;
          continue
        }else {
          return tcoll__19236
        }
        break
      }
    }
  }else {
    throw new Error("conj! after persistent");
  }
};
cljs.core.TransientHashMap.prototype.assoc_BANG_ = function(k, v) {
  var this__19239 = this;
  var tcoll__19240 = this;
  if(this__19239.edit) {
    if(k == null) {
      if(this__19239.nil_val === v) {
      }else {
        this__19239.nil_val = v
      }
      if(this__19239.has_nil_QMARK_) {
      }else {
        this__19239.count = this__19239.count + 1;
        this__19239.has_nil_QMARK_ = true
      }
      return tcoll__19240
    }else {
      var added_leaf_QMARK___19241 = new cljs.core.Box(false);
      var node__19242 = (this__19239.root == null ? cljs.core.BitmapIndexedNode.EMPTY : this__19239.root).inode_assoc_BANG_(this__19239.edit, 0, cljs.core.hash.call(null, k), k, v, added_leaf_QMARK___19241);
      if(node__19242 === this__19239.root) {
      }else {
        this__19239.root = node__19242
      }
      if(added_leaf_QMARK___19241.val) {
        this__19239.count = this__19239.count + 1
      }else {
      }
      return tcoll__19240
    }
  }else {
    throw new Error("assoc! after persistent!");
  }
};
cljs.core.TransientHashMap.prototype.without_BANG_ = function(k) {
  var this__19243 = this;
  var tcoll__19244 = this;
  if(this__19243.edit) {
    if(k == null) {
      if(this__19243.has_nil_QMARK_) {
        this__19243.has_nil_QMARK_ = false;
        this__19243.nil_val = null;
        this__19243.count = this__19243.count - 1;
        return tcoll__19244
      }else {
        return tcoll__19244
      }
    }else {
      if(this__19243.root == null) {
        return tcoll__19244
      }else {
        var removed_leaf_QMARK___19245 = new cljs.core.Box(false);
        var node__19246 = this__19243.root.inode_without_BANG_(this__19243.edit, 0, cljs.core.hash.call(null, k), k, removed_leaf_QMARK___19245);
        if(node__19246 === this__19243.root) {
        }else {
          this__19243.root = node__19246
        }
        if(cljs.core.truth_(removed_leaf_QMARK___19245[0])) {
          this__19243.count = this__19243.count - 1
        }else {
        }
        return tcoll__19244
      }
    }
  }else {
    throw new Error("dissoc! after persistent!");
  }
};
cljs.core.TransientHashMap.prototype.persistent_BANG_ = function() {
  var this__19247 = this;
  var tcoll__19248 = this;
  if(this__19247.edit) {
    this__19247.edit = null;
    return new cljs.core.PersistentHashMap(null, this__19247.count, this__19247.root, this__19247.has_nil_QMARK_, this__19247.nil_val, null)
  }else {
    throw new Error("persistent! called twice");
  }
};
cljs.core.TransientHashMap;
cljs.core.tree_map_seq_push = function tree_map_seq_push(node, stack, ascending_QMARK_) {
  var t__19253 = node;
  var stack__19254 = stack;
  while(true) {
    if(!(t__19253 == null)) {
      var G__19255 = ascending_QMARK_ ? t__19253.left : t__19253.right;
      var G__19256 = cljs.core.conj.call(null, stack__19254, t__19253);
      t__19253 = G__19255;
      stack__19254 = G__19256;
      continue
    }else {
      return stack__19254
    }
    break
  }
};
goog.provide("cljs.core.PersistentTreeMapSeq");
cljs.core.PersistentTreeMapSeq = function(meta, stack, ascending_QMARK_, cnt, __hash) {
  this.meta = meta;
  this.stack = stack;
  this.ascending_QMARK_ = ascending_QMARK_;
  this.cnt = cnt;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 31850574
};
cljs.core.PersistentTreeMapSeq.cljs$lang$type = true;
cljs.core.PersistentTreeMapSeq.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentTreeMapSeq")
};
cljs.core.PersistentTreeMapSeq.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentTreeMapSeq")
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19257 = this;
  var h__2247__auto____19258 = this__19257.__hash;
  if(!(h__2247__auto____19258 == null)) {
    return h__2247__auto____19258
  }else {
    var h__2247__auto____19259 = cljs.core.hash_coll.call(null, coll);
    this__19257.__hash = h__2247__auto____19259;
    return h__2247__auto____19259
  }
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19260 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.PersistentTreeMapSeq.prototype.toString = function() {
  var this__19261 = this;
  var this__19262 = this;
  return cljs.core.pr_str.call(null, this__19262)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__19263 = this;
  return this$
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19264 = this;
  if(this__19264.cnt < 0) {
    return cljs.core.count.call(null, cljs.core.next.call(null, coll)) + 1
  }else {
    return this__19264.cnt
  }
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(this$) {
  var this__19265 = this;
  return cljs.core.peek.call(null, this__19265.stack)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(this$) {
  var this__19266 = this;
  var t__19267 = cljs.core.first.call(null, this__19266.stack);
  var next_stack__19268 = cljs.core.tree_map_seq_push.call(null, this__19266.ascending_QMARK_ ? t__19267.right : t__19267.left, cljs.core.next.call(null, this__19266.stack), this__19266.ascending_QMARK_);
  if(!(next_stack__19268 == null)) {
    return new cljs.core.PersistentTreeMapSeq(null, next_stack__19268, this__19266.ascending_QMARK_, this__19266.cnt - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19269 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19270 = this;
  return new cljs.core.PersistentTreeMapSeq(meta, this__19270.stack, this__19270.ascending_QMARK_, this__19270.cnt, this__19270.__hash)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19271 = this;
  return this__19271.meta
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19272 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19272.meta)
};
cljs.core.PersistentTreeMapSeq;
cljs.core.create_tree_map_seq = function create_tree_map_seq(tree, ascending_QMARK_, cnt) {
  return new cljs.core.PersistentTreeMapSeq(null, cljs.core.tree_map_seq_push.call(null, tree, null, ascending_QMARK_), ascending_QMARK_, cnt, null)
};
cljs.core.balance_left = function balance_left(key, val, ins, right) {
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, ins)) {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, ins.left)) {
      return new cljs.core.RedNode(ins.key, ins.val, ins.left.blacken(), new cljs.core.BlackNode(key, val, ins.right, right, null), null)
    }else {
      if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, ins.right)) {
        return new cljs.core.RedNode(ins.right.key, ins.right.val, new cljs.core.BlackNode(ins.key, ins.val, ins.left, ins.right.left, null), new cljs.core.BlackNode(key, val, ins.right.right, right, null), null)
      }else {
        if("\ufdd0'else") {
          return new cljs.core.BlackNode(key, val, ins, right, null)
        }else {
          return null
        }
      }
    }
  }else {
    return new cljs.core.BlackNode(key, val, ins, right, null)
  }
};
cljs.core.balance_right = function balance_right(key, val, left, ins) {
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, ins)) {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, ins.right)) {
      return new cljs.core.RedNode(ins.key, ins.val, new cljs.core.BlackNode(key, val, left, ins.left, null), ins.right.blacken(), null)
    }else {
      if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, ins.left)) {
        return new cljs.core.RedNode(ins.left.key, ins.left.val, new cljs.core.BlackNode(key, val, left, ins.left.left, null), new cljs.core.BlackNode(ins.key, ins.val, ins.left.right, ins.right, null), null)
      }else {
        if("\ufdd0'else") {
          return new cljs.core.BlackNode(key, val, left, ins, null)
        }else {
          return null
        }
      }
    }
  }else {
    return new cljs.core.BlackNode(key, val, left, ins, null)
  }
};
cljs.core.balance_left_del = function balance_left_del(key, val, del, right) {
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, del)) {
    return new cljs.core.RedNode(key, val, del.blacken(), right, null)
  }else {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, right)) {
      return cljs.core.balance_right.call(null, key, val, del, right.redden())
    }else {
      if(function() {
        var and__3822__auto____19274 = cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, right);
        if(and__3822__auto____19274) {
          return cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, right.left)
        }else {
          return and__3822__auto____19274
        }
      }()) {
        return new cljs.core.RedNode(right.left.key, right.left.val, new cljs.core.BlackNode(key, val, del, right.left.left, null), cljs.core.balance_right.call(null, right.key, right.val, right.left.right, right.right.redden()), null)
      }else {
        if("\ufdd0'else") {
          throw new Error("red-black tree invariant violation");
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.balance_right_del = function balance_right_del(key, val, left, del) {
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, del)) {
    return new cljs.core.RedNode(key, val, left, del.blacken(), null)
  }else {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, left)) {
      return cljs.core.balance_left.call(null, key, val, left.redden(), del)
    }else {
      if(function() {
        var and__3822__auto____19276 = cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, left);
        if(and__3822__auto____19276) {
          return cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, left.right)
        }else {
          return and__3822__auto____19276
        }
      }()) {
        return new cljs.core.RedNode(left.right.key, left.right.val, cljs.core.balance_left.call(null, left.key, left.val, left.left.redden(), left.right.left), new cljs.core.BlackNode(key, val, left.right.right, del, null), null)
      }else {
        if("\ufdd0'else") {
          throw new Error("red-black tree invariant violation");
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.tree_map_kv_reduce = function tree_map_kv_reduce(node, f, init) {
  var init__19280 = f.call(null, init, node.key, node.val);
  if(cljs.core.reduced_QMARK_.call(null, init__19280)) {
    return cljs.core.deref.call(null, init__19280)
  }else {
    var init__19281 = !(node.left == null) ? tree_map_kv_reduce.call(null, node.left, f, init__19280) : init__19280;
    if(cljs.core.reduced_QMARK_.call(null, init__19281)) {
      return cljs.core.deref.call(null, init__19281)
    }else {
      var init__19282 = !(node.right == null) ? tree_map_kv_reduce.call(null, node.right, f, init__19281) : init__19281;
      if(cljs.core.reduced_QMARK_.call(null, init__19282)) {
        return cljs.core.deref.call(null, init__19282)
      }else {
        return init__19282
      }
    }
  }
};
goog.provide("cljs.core.BlackNode");
cljs.core.BlackNode = function(key, val, left, right, __hash) {
  this.key = key;
  this.val = val;
  this.left = left;
  this.right = right;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32402207
};
cljs.core.BlackNode.cljs$lang$type = true;
cljs.core.BlackNode.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/BlackNode")
};
cljs.core.BlackNode.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/BlackNode")
};
cljs.core.BlackNode.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19285 = this;
  var h__2247__auto____19286 = this__19285.__hash;
  if(!(h__2247__auto____19286 == null)) {
    return h__2247__auto____19286
  }else {
    var h__2247__auto____19287 = cljs.core.hash_coll.call(null, coll);
    this__19285.__hash = h__2247__auto____19287;
    return h__2247__auto____19287
  }
};
cljs.core.BlackNode.prototype.cljs$core$ILookup$_lookup$arity$2 = function(node, k) {
  var this__19288 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, null)
};
cljs.core.BlackNode.prototype.cljs$core$ILookup$_lookup$arity$3 = function(node, k, not_found) {
  var this__19289 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, not_found)
};
cljs.core.BlackNode.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(node, k, v) {
  var this__19290 = this;
  return cljs.core.assoc.call(null, cljs.core.PersistentVector.fromArray([this__19290.key, this__19290.val], true), k, v)
};
cljs.core.BlackNode.prototype.call = function() {
  var G__19338 = null;
  var G__19338__2 = function(this_sym19291, k) {
    var this__19293 = this;
    var this_sym19291__19294 = this;
    var node__19295 = this_sym19291__19294;
    return node__19295.cljs$core$ILookup$_lookup$arity$2(node__19295, k)
  };
  var G__19338__3 = function(this_sym19292, k, not_found) {
    var this__19293 = this;
    var this_sym19292__19296 = this;
    var node__19297 = this_sym19292__19296;
    return node__19297.cljs$core$ILookup$_lookup$arity$3(node__19297, k, not_found)
  };
  G__19338 = function(this_sym19292, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19338__2.call(this, this_sym19292, k);
      case 3:
        return G__19338__3.call(this, this_sym19292, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19338
}();
cljs.core.BlackNode.prototype.apply = function(this_sym19283, args19284) {
  var this__19298 = this;
  return this_sym19283.call.apply(this_sym19283, [this_sym19283].concat(args19284.slice()))
};
cljs.core.BlackNode.prototype.cljs$core$ICollection$_conj$arity$2 = function(node, o) {
  var this__19299 = this;
  return cljs.core.PersistentVector.fromArray([this__19299.key, this__19299.val, o], true)
};
cljs.core.BlackNode.prototype.cljs$core$IMapEntry$_key$arity$1 = function(node) {
  var this__19300 = this;
  return this__19300.key
};
cljs.core.BlackNode.prototype.cljs$core$IMapEntry$_val$arity$1 = function(node) {
  var this__19301 = this;
  return this__19301.val
};
cljs.core.BlackNode.prototype.add_right = function(ins) {
  var this__19302 = this;
  var node__19303 = this;
  return ins.balance_right(node__19303)
};
cljs.core.BlackNode.prototype.redden = function() {
  var this__19304 = this;
  var node__19305 = this;
  return new cljs.core.RedNode(this__19304.key, this__19304.val, this__19304.left, this__19304.right, null)
};
cljs.core.BlackNode.prototype.remove_right = function(del) {
  var this__19306 = this;
  var node__19307 = this;
  return cljs.core.balance_right_del.call(null, this__19306.key, this__19306.val, this__19306.left, del)
};
cljs.core.BlackNode.prototype.replace = function(key, val, left, right) {
  var this__19308 = this;
  var node__19309 = this;
  return new cljs.core.BlackNode(key, val, left, right, null)
};
cljs.core.BlackNode.prototype.kv_reduce = function(f, init) {
  var this__19310 = this;
  var node__19311 = this;
  return cljs.core.tree_map_kv_reduce.call(null, node__19311, f, init)
};
cljs.core.BlackNode.prototype.remove_left = function(del) {
  var this__19312 = this;
  var node__19313 = this;
  return cljs.core.balance_left_del.call(null, this__19312.key, this__19312.val, del, this__19312.right)
};
cljs.core.BlackNode.prototype.add_left = function(ins) {
  var this__19314 = this;
  var node__19315 = this;
  return ins.balance_left(node__19315)
};
cljs.core.BlackNode.prototype.balance_left = function(parent) {
  var this__19316 = this;
  var node__19317 = this;
  return new cljs.core.BlackNode(parent.key, parent.val, node__19317, parent.right, null)
};
cljs.core.BlackNode.prototype.toString = function() {
  var G__19339 = null;
  var G__19339__0 = function() {
    var this__19318 = this;
    var this__19320 = this;
    return cljs.core.pr_str.call(null, this__19320)
  };
  G__19339 = function() {
    switch(arguments.length) {
      case 0:
        return G__19339__0.call(this)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19339
}();
cljs.core.BlackNode.prototype.balance_right = function(parent) {
  var this__19321 = this;
  var node__19322 = this;
  return new cljs.core.BlackNode(parent.key, parent.val, parent.left, node__19322, null)
};
cljs.core.BlackNode.prototype.blacken = function() {
  var this__19323 = this;
  var node__19324 = this;
  return node__19324
};
cljs.core.BlackNode.prototype.cljs$core$IReduce$_reduce$arity$2 = function(node, f) {
  var this__19325 = this;
  return cljs.core.ci_reduce.call(null, node, f)
};
cljs.core.BlackNode.prototype.cljs$core$IReduce$_reduce$arity$3 = function(node, f, start) {
  var this__19326 = this;
  return cljs.core.ci_reduce.call(null, node, f, start)
};
cljs.core.BlackNode.prototype.cljs$core$ISeqable$_seq$arity$1 = function(node) {
  var this__19327 = this;
  return cljs.core.list.call(null, this__19327.key, this__19327.val)
};
cljs.core.BlackNode.prototype.cljs$core$ICounted$_count$arity$1 = function(node) {
  var this__19328 = this;
  return 2
};
cljs.core.BlackNode.prototype.cljs$core$IStack$_peek$arity$1 = function(node) {
  var this__19329 = this;
  return this__19329.val
};
cljs.core.BlackNode.prototype.cljs$core$IStack$_pop$arity$1 = function(node) {
  var this__19330 = this;
  return cljs.core.PersistentVector.fromArray([this__19330.key], true)
};
cljs.core.BlackNode.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(node, n, v) {
  var this__19331 = this;
  return cljs.core._assoc_n.call(null, cljs.core.PersistentVector.fromArray([this__19331.key, this__19331.val], true), n, v)
};
cljs.core.BlackNode.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19332 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.BlackNode.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(node, meta) {
  var this__19333 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.fromArray([this__19333.key, this__19333.val], true), meta)
};
cljs.core.BlackNode.prototype.cljs$core$IMeta$_meta$arity$1 = function(node) {
  var this__19334 = this;
  return null
};
cljs.core.BlackNode.prototype.cljs$core$IIndexed$_nth$arity$2 = function(node, n) {
  var this__19335 = this;
  if(n === 0) {
    return this__19335.key
  }else {
    if(n === 1) {
      return this__19335.val
    }else {
      if("\ufdd0'else") {
        return null
      }else {
        return null
      }
    }
  }
};
cljs.core.BlackNode.prototype.cljs$core$IIndexed$_nth$arity$3 = function(node, n, not_found) {
  var this__19336 = this;
  if(n === 0) {
    return this__19336.key
  }else {
    if(n === 1) {
      return this__19336.val
    }else {
      if("\ufdd0'else") {
        return not_found
      }else {
        return null
      }
    }
  }
};
cljs.core.BlackNode.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(node) {
  var this__19337 = this;
  return cljs.core.PersistentVector.EMPTY
};
cljs.core.BlackNode;
goog.provide("cljs.core.RedNode");
cljs.core.RedNode = function(key, val, left, right, __hash) {
  this.key = key;
  this.val = val;
  this.left = left;
  this.right = right;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32402207
};
cljs.core.RedNode.cljs$lang$type = true;
cljs.core.RedNode.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/RedNode")
};
cljs.core.RedNode.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/RedNode")
};
cljs.core.RedNode.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19342 = this;
  var h__2247__auto____19343 = this__19342.__hash;
  if(!(h__2247__auto____19343 == null)) {
    return h__2247__auto____19343
  }else {
    var h__2247__auto____19344 = cljs.core.hash_coll.call(null, coll);
    this__19342.__hash = h__2247__auto____19344;
    return h__2247__auto____19344
  }
};
cljs.core.RedNode.prototype.cljs$core$ILookup$_lookup$arity$2 = function(node, k) {
  var this__19345 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, null)
};
cljs.core.RedNode.prototype.cljs$core$ILookup$_lookup$arity$3 = function(node, k, not_found) {
  var this__19346 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, not_found)
};
cljs.core.RedNode.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(node, k, v) {
  var this__19347 = this;
  return cljs.core.assoc.call(null, cljs.core.PersistentVector.fromArray([this__19347.key, this__19347.val], true), k, v)
};
cljs.core.RedNode.prototype.call = function() {
  var G__19395 = null;
  var G__19395__2 = function(this_sym19348, k) {
    var this__19350 = this;
    var this_sym19348__19351 = this;
    var node__19352 = this_sym19348__19351;
    return node__19352.cljs$core$ILookup$_lookup$arity$2(node__19352, k)
  };
  var G__19395__3 = function(this_sym19349, k, not_found) {
    var this__19350 = this;
    var this_sym19349__19353 = this;
    var node__19354 = this_sym19349__19353;
    return node__19354.cljs$core$ILookup$_lookup$arity$3(node__19354, k, not_found)
  };
  G__19395 = function(this_sym19349, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19395__2.call(this, this_sym19349, k);
      case 3:
        return G__19395__3.call(this, this_sym19349, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19395
}();
cljs.core.RedNode.prototype.apply = function(this_sym19340, args19341) {
  var this__19355 = this;
  return this_sym19340.call.apply(this_sym19340, [this_sym19340].concat(args19341.slice()))
};
cljs.core.RedNode.prototype.cljs$core$ICollection$_conj$arity$2 = function(node, o) {
  var this__19356 = this;
  return cljs.core.PersistentVector.fromArray([this__19356.key, this__19356.val, o], true)
};
cljs.core.RedNode.prototype.cljs$core$IMapEntry$_key$arity$1 = function(node) {
  var this__19357 = this;
  return this__19357.key
};
cljs.core.RedNode.prototype.cljs$core$IMapEntry$_val$arity$1 = function(node) {
  var this__19358 = this;
  return this__19358.val
};
cljs.core.RedNode.prototype.add_right = function(ins) {
  var this__19359 = this;
  var node__19360 = this;
  return new cljs.core.RedNode(this__19359.key, this__19359.val, this__19359.left, ins, null)
};
cljs.core.RedNode.prototype.redden = function() {
  var this__19361 = this;
  var node__19362 = this;
  throw new Error("red-black tree invariant violation");
};
cljs.core.RedNode.prototype.remove_right = function(del) {
  var this__19363 = this;
  var node__19364 = this;
  return new cljs.core.RedNode(this__19363.key, this__19363.val, this__19363.left, del, null)
};
cljs.core.RedNode.prototype.replace = function(key, val, left, right) {
  var this__19365 = this;
  var node__19366 = this;
  return new cljs.core.RedNode(key, val, left, right, null)
};
cljs.core.RedNode.prototype.kv_reduce = function(f, init) {
  var this__19367 = this;
  var node__19368 = this;
  return cljs.core.tree_map_kv_reduce.call(null, node__19368, f, init)
};
cljs.core.RedNode.prototype.remove_left = function(del) {
  var this__19369 = this;
  var node__19370 = this;
  return new cljs.core.RedNode(this__19369.key, this__19369.val, del, this__19369.right, null)
};
cljs.core.RedNode.prototype.add_left = function(ins) {
  var this__19371 = this;
  var node__19372 = this;
  return new cljs.core.RedNode(this__19371.key, this__19371.val, ins, this__19371.right, null)
};
cljs.core.RedNode.prototype.balance_left = function(parent) {
  var this__19373 = this;
  var node__19374 = this;
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19373.left)) {
    return new cljs.core.RedNode(this__19373.key, this__19373.val, this__19373.left.blacken(), new cljs.core.BlackNode(parent.key, parent.val, this__19373.right, parent.right, null), null)
  }else {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19373.right)) {
      return new cljs.core.RedNode(this__19373.right.key, this__19373.right.val, new cljs.core.BlackNode(this__19373.key, this__19373.val, this__19373.left, this__19373.right.left, null), new cljs.core.BlackNode(parent.key, parent.val, this__19373.right.right, parent.right, null), null)
    }else {
      if("\ufdd0'else") {
        return new cljs.core.BlackNode(parent.key, parent.val, node__19374, parent.right, null)
      }else {
        return null
      }
    }
  }
};
cljs.core.RedNode.prototype.toString = function() {
  var G__19396 = null;
  var G__19396__0 = function() {
    var this__19375 = this;
    var this__19377 = this;
    return cljs.core.pr_str.call(null, this__19377)
  };
  G__19396 = function() {
    switch(arguments.length) {
      case 0:
        return G__19396__0.call(this)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19396
}();
cljs.core.RedNode.prototype.balance_right = function(parent) {
  var this__19378 = this;
  var node__19379 = this;
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19378.right)) {
    return new cljs.core.RedNode(this__19378.key, this__19378.val, new cljs.core.BlackNode(parent.key, parent.val, parent.left, this__19378.left, null), this__19378.right.blacken(), null)
  }else {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19378.left)) {
      return new cljs.core.RedNode(this__19378.left.key, this__19378.left.val, new cljs.core.BlackNode(parent.key, parent.val, parent.left, this__19378.left.left, null), new cljs.core.BlackNode(this__19378.key, this__19378.val, this__19378.left.right, this__19378.right, null), null)
    }else {
      if("\ufdd0'else") {
        return new cljs.core.BlackNode(parent.key, parent.val, parent.left, node__19379, null)
      }else {
        return null
      }
    }
  }
};
cljs.core.RedNode.prototype.blacken = function() {
  var this__19380 = this;
  var node__19381 = this;
  return new cljs.core.BlackNode(this__19380.key, this__19380.val, this__19380.left, this__19380.right, null)
};
cljs.core.RedNode.prototype.cljs$core$IReduce$_reduce$arity$2 = function(node, f) {
  var this__19382 = this;
  return cljs.core.ci_reduce.call(null, node, f)
};
cljs.core.RedNode.prototype.cljs$core$IReduce$_reduce$arity$3 = function(node, f, start) {
  var this__19383 = this;
  return cljs.core.ci_reduce.call(null, node, f, start)
};
cljs.core.RedNode.prototype.cljs$core$ISeqable$_seq$arity$1 = function(node) {
  var this__19384 = this;
  return cljs.core.list.call(null, this__19384.key, this__19384.val)
};
cljs.core.RedNode.prototype.cljs$core$ICounted$_count$arity$1 = function(node) {
  var this__19385 = this;
  return 2
};
cljs.core.RedNode.prototype.cljs$core$IStack$_peek$arity$1 = function(node) {
  var this__19386 = this;
  return this__19386.val
};
cljs.core.RedNode.prototype.cljs$core$IStack$_pop$arity$1 = function(node) {
  var this__19387 = this;
  return cljs.core.PersistentVector.fromArray([this__19387.key], true)
};
cljs.core.RedNode.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(node, n, v) {
  var this__19388 = this;
  return cljs.core._assoc_n.call(null, cljs.core.PersistentVector.fromArray([this__19388.key, this__19388.val], true), n, v)
};
cljs.core.RedNode.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19389 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.RedNode.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(node, meta) {
  var this__19390 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.fromArray([this__19390.key, this__19390.val], true), meta)
};
cljs.core.RedNode.prototype.cljs$core$IMeta$_meta$arity$1 = function(node) {
  var this__19391 = this;
  return null
};
cljs.core.RedNode.prototype.cljs$core$IIndexed$_nth$arity$2 = function(node, n) {
  var this__19392 = this;
  if(n === 0) {
    return this__19392.key
  }else {
    if(n === 1) {
      return this__19392.val
    }else {
      if("\ufdd0'else") {
        return null
      }else {
        return null
      }
    }
  }
};
cljs.core.RedNode.prototype.cljs$core$IIndexed$_nth$arity$3 = function(node, n, not_found) {
  var this__19393 = this;
  if(n === 0) {
    return this__19393.key
  }else {
    if(n === 1) {
      return this__19393.val
    }else {
      if("\ufdd0'else") {
        return not_found
      }else {
        return null
      }
    }
  }
};
cljs.core.RedNode.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(node) {
  var this__19394 = this;
  return cljs.core.PersistentVector.EMPTY
};
cljs.core.RedNode;
cljs.core.tree_map_add = function tree_map_add(comp, tree, k, v, found) {
  if(tree == null) {
    return new cljs.core.RedNode(k, v, null, null, null)
  }else {
    var c__19400 = comp.call(null, k, tree.key);
    if(c__19400 === 0) {
      found[0] = tree;
      return null
    }else {
      if(c__19400 < 0) {
        var ins__19401 = tree_map_add.call(null, comp, tree.left, k, v, found);
        if(!(ins__19401 == null)) {
          return tree.add_left(ins__19401)
        }else {
          return null
        }
      }else {
        if("\ufdd0'else") {
          var ins__19402 = tree_map_add.call(null, comp, tree.right, k, v, found);
          if(!(ins__19402 == null)) {
            return tree.add_right(ins__19402)
          }else {
            return null
          }
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.tree_map_append = function tree_map_append(left, right) {
  if(left == null) {
    return right
  }else {
    if(right == null) {
      return left
    }else {
      if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, left)) {
        if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, right)) {
          var app__19405 = tree_map_append.call(null, left.right, right.left);
          if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, app__19405)) {
            return new cljs.core.RedNode(app__19405.key, app__19405.val, new cljs.core.RedNode(left.key, left.val, left.left, app__19405.left, null), new cljs.core.RedNode(right.key, right.val, app__19405.right, right.right, null), null)
          }else {
            return new cljs.core.RedNode(left.key, left.val, left.left, new cljs.core.RedNode(right.key, right.val, app__19405, right.right, null), null)
          }
        }else {
          return new cljs.core.RedNode(left.key, left.val, left.left, tree_map_append.call(null, left.right, right), null)
        }
      }else {
        if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, right)) {
          return new cljs.core.RedNode(right.key, right.val, tree_map_append.call(null, left, right.left), right.right, null)
        }else {
          if("\ufdd0'else") {
            var app__19406 = tree_map_append.call(null, left.right, right.left);
            if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, app__19406)) {
              return new cljs.core.RedNode(app__19406.key, app__19406.val, new cljs.core.BlackNode(left.key, left.val, left.left, app__19406.left, null), new cljs.core.BlackNode(right.key, right.val, app__19406.right, right.right, null), null)
            }else {
              return cljs.core.balance_left_del.call(null, left.key, left.val, left.left, new cljs.core.BlackNode(right.key, right.val, app__19406, right.right, null))
            }
          }else {
            return null
          }
        }
      }
    }
  }
};
cljs.core.tree_map_remove = function tree_map_remove(comp, tree, k, found) {
  if(!(tree == null)) {
    var c__19412 = comp.call(null, k, tree.key);
    if(c__19412 === 0) {
      found[0] = tree;
      return cljs.core.tree_map_append.call(null, tree.left, tree.right)
    }else {
      if(c__19412 < 0) {
        var del__19413 = tree_map_remove.call(null, comp, tree.left, k, found);
        if(function() {
          var or__3824__auto____19414 = !(del__19413 == null);
          if(or__3824__auto____19414) {
            return or__3824__auto____19414
          }else {
            return!(found[0] == null)
          }
        }()) {
          if(cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, tree.left)) {
            return cljs.core.balance_left_del.call(null, tree.key, tree.val, del__19413, tree.right)
          }else {
            return new cljs.core.RedNode(tree.key, tree.val, del__19413, tree.right, null)
          }
        }else {
          return null
        }
      }else {
        if("\ufdd0'else") {
          var del__19415 = tree_map_remove.call(null, comp, tree.right, k, found);
          if(function() {
            var or__3824__auto____19416 = !(del__19415 == null);
            if(or__3824__auto____19416) {
              return or__3824__auto____19416
            }else {
              return!(found[0] == null)
            }
          }()) {
            if(cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, tree.right)) {
              return cljs.core.balance_right_del.call(null, tree.key, tree.val, tree.left, del__19415)
            }else {
              return new cljs.core.RedNode(tree.key, tree.val, tree.left, del__19415, null)
            }
          }else {
            return null
          }
        }else {
          return null
        }
      }
    }
  }else {
    return null
  }
};
cljs.core.tree_map_replace = function tree_map_replace(comp, tree, k, v) {
  var tk__19419 = tree.key;
  var c__19420 = comp.call(null, k, tk__19419);
  if(c__19420 === 0) {
    return tree.replace(tk__19419, v, tree.left, tree.right)
  }else {
    if(c__19420 < 0) {
      return tree.replace(tk__19419, tree.val, tree_map_replace.call(null, comp, tree.left, k, v), tree.right)
    }else {
      if("\ufdd0'else") {
        return tree.replace(tk__19419, tree.val, tree.left, tree_map_replace.call(null, comp, tree.right, k, v))
      }else {
        return null
      }
    }
  }
};
goog.provide("cljs.core.PersistentTreeMap");
cljs.core.PersistentTreeMap = function(comp, tree, cnt, meta, __hash) {
  this.comp = comp;
  this.tree = tree;
  this.cnt = cnt;
  this.meta = meta;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 418776847
};
cljs.core.PersistentTreeMap.cljs$lang$type = true;
cljs.core.PersistentTreeMap.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentTreeMap")
};
cljs.core.PersistentTreeMap.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentTreeMap")
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19423 = this;
  var h__2247__auto____19424 = this__19423.__hash;
  if(!(h__2247__auto____19424 == null)) {
    return h__2247__auto____19424
  }else {
    var h__2247__auto____19425 = cljs.core.hash_imap.call(null, coll);
    this__19423.__hash = h__2247__auto____19425;
    return h__2247__auto____19425
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19426 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19427 = this;
  var n__19428 = coll.entry_at(k);
  if(!(n__19428 == null)) {
    return n__19428.val
  }else {
    return not_found
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19429 = this;
  var found__19430 = [null];
  var t__19431 = cljs.core.tree_map_add.call(null, this__19429.comp, this__19429.tree, k, v, found__19430);
  if(t__19431 == null) {
    var found_node__19432 = cljs.core.nth.call(null, found__19430, 0);
    if(cljs.core._EQ_.call(null, v, found_node__19432.val)) {
      return coll
    }else {
      return new cljs.core.PersistentTreeMap(this__19429.comp, cljs.core.tree_map_replace.call(null, this__19429.comp, this__19429.tree, k, v), this__19429.cnt, this__19429.meta, null)
    }
  }else {
    return new cljs.core.PersistentTreeMap(this__19429.comp, t__19431.blacken(), this__19429.cnt + 1, this__19429.meta, null)
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19433 = this;
  return!(coll.entry_at(k) == null)
};
cljs.core.PersistentTreeMap.prototype.call = function() {
  var G__19467 = null;
  var G__19467__2 = function(this_sym19434, k) {
    var this__19436 = this;
    var this_sym19434__19437 = this;
    var coll__19438 = this_sym19434__19437;
    return coll__19438.cljs$core$ILookup$_lookup$arity$2(coll__19438, k)
  };
  var G__19467__3 = function(this_sym19435, k, not_found) {
    var this__19436 = this;
    var this_sym19435__19439 = this;
    var coll__19440 = this_sym19435__19439;
    return coll__19440.cljs$core$ILookup$_lookup$arity$3(coll__19440, k, not_found)
  };
  G__19467 = function(this_sym19435, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19467__2.call(this, this_sym19435, k);
      case 3:
        return G__19467__3.call(this, this_sym19435, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19467
}();
cljs.core.PersistentTreeMap.prototype.apply = function(this_sym19421, args19422) {
  var this__19441 = this;
  return this_sym19421.call.apply(this_sym19421, [this_sym19421].concat(args19422.slice()))
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(coll, f, init) {
  var this__19442 = this;
  if(!(this__19442.tree == null)) {
    return cljs.core.tree_map_kv_reduce.call(null, this__19442.tree, f, init)
  }else {
    return init
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19443 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__19444 = this;
  if(this__19444.cnt > 0) {
    return cljs.core.create_tree_map_seq.call(null, this__19444.tree, false, this__19444.cnt)
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.toString = function() {
  var this__19445 = this;
  var this__19446 = this;
  return cljs.core.pr_str.call(null, this__19446)
};
cljs.core.PersistentTreeMap.prototype.entry_at = function(k) {
  var this__19447 = this;
  var coll__19448 = this;
  var t__19449 = this__19447.tree;
  while(true) {
    if(!(t__19449 == null)) {
      var c__19450 = this__19447.comp.call(null, k, t__19449.key);
      if(c__19450 === 0) {
        return t__19449
      }else {
        if(c__19450 < 0) {
          var G__19468 = t__19449.left;
          t__19449 = G__19468;
          continue
        }else {
          if("\ufdd0'else") {
            var G__19469 = t__19449.right;
            t__19449 = G__19469;
            continue
          }else {
            return null
          }
        }
      }
    }else {
      return null
    }
    break
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_sorted_seq$arity$2 = function(coll, ascending_QMARK_) {
  var this__19451 = this;
  if(this__19451.cnt > 0) {
    return cljs.core.create_tree_map_seq.call(null, this__19451.tree, ascending_QMARK_, this__19451.cnt)
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_sorted_seq_from$arity$3 = function(coll, k, ascending_QMARK_) {
  var this__19452 = this;
  if(this__19452.cnt > 0) {
    var stack__19453 = null;
    var t__19454 = this__19452.tree;
    while(true) {
      if(!(t__19454 == null)) {
        var c__19455 = this__19452.comp.call(null, k, t__19454.key);
        if(c__19455 === 0) {
          return new cljs.core.PersistentTreeMapSeq(null, cljs.core.conj.call(null, stack__19453, t__19454), ascending_QMARK_, -1, null)
        }else {
          if(cljs.core.truth_(ascending_QMARK_)) {
            if(c__19455 < 0) {
              var G__19470 = cljs.core.conj.call(null, stack__19453, t__19454);
              var G__19471 = t__19454.left;
              stack__19453 = G__19470;
              t__19454 = G__19471;
              continue
            }else {
              var G__19472 = stack__19453;
              var G__19473 = t__19454.right;
              stack__19453 = G__19472;
              t__19454 = G__19473;
              continue
            }
          }else {
            if("\ufdd0'else") {
              if(c__19455 > 0) {
                var G__19474 = cljs.core.conj.call(null, stack__19453, t__19454);
                var G__19475 = t__19454.right;
                stack__19453 = G__19474;
                t__19454 = G__19475;
                continue
              }else {
                var G__19476 = stack__19453;
                var G__19477 = t__19454.left;
                stack__19453 = G__19476;
                t__19454 = G__19477;
                continue
              }
            }else {
              return null
            }
          }
        }
      }else {
        if(stack__19453 == null) {
          return new cljs.core.PersistentTreeMapSeq(null, stack__19453, ascending_QMARK_, -1, null)
        }else {
          return null
        }
      }
      break
    }
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_entry_key$arity$2 = function(coll, entry) {
  var this__19456 = this;
  return cljs.core.key.call(null, entry)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_comparator$arity$1 = function(coll) {
  var this__19457 = this;
  return this__19457.comp
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19458 = this;
  if(this__19458.cnt > 0) {
    return cljs.core.create_tree_map_seq.call(null, this__19458.tree, true, this__19458.cnt)
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19459 = this;
  return this__19459.cnt
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19460 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19461 = this;
  return new cljs.core.PersistentTreeMap(this__19461.comp, this__19461.tree, this__19461.cnt, meta, this__19461.__hash)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19462 = this;
  return this__19462.meta
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19463 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentTreeMap.EMPTY, this__19463.meta)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19464 = this;
  var found__19465 = [null];
  var t__19466 = cljs.core.tree_map_remove.call(null, this__19464.comp, this__19464.tree, k, found__19465);
  if(t__19466 == null) {
    if(cljs.core.nth.call(null, found__19465, 0) == null) {
      return coll
    }else {
      return new cljs.core.PersistentTreeMap(this__19464.comp, null, 0, this__19464.meta, null)
    }
  }else {
    return new cljs.core.PersistentTreeMap(this__19464.comp, t__19466.blacken(), this__19464.cnt - 1, this__19464.meta, null)
  }
};
cljs.core.PersistentTreeMap;
cljs.core.PersistentTreeMap.EMPTY = new cljs.core.PersistentTreeMap(cljs.core.compare, null, 0, null, 0);
cljs.core.hash_map = function() {
  var hash_map__delegate = function(keyvals) {
    var in__19480 = cljs.core.seq.call(null, keyvals);
    var out__19481 = cljs.core.transient$.call(null, cljs.core.PersistentHashMap.EMPTY);
    while(true) {
      if(in__19480) {
        var G__19482 = cljs.core.nnext.call(null, in__19480);
        var G__19483 = cljs.core.assoc_BANG_.call(null, out__19481, cljs.core.first.call(null, in__19480), cljs.core.second.call(null, in__19480));
        in__19480 = G__19482;
        out__19481 = G__19483;
        continue
      }else {
        return cljs.core.persistent_BANG_.call(null, out__19481)
      }
      break
    }
  };
  var hash_map = function(var_args) {
    var keyvals = null;
    if(goog.isDef(var_args)) {
      keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return hash_map__delegate.call(this, keyvals)
  };
  hash_map.cljs$lang$maxFixedArity = 0;
  hash_map.cljs$lang$applyTo = function(arglist__19484) {
    var keyvals = cljs.core.seq(arglist__19484);
    return hash_map__delegate(keyvals)
  };
  hash_map.cljs$lang$arity$variadic = hash_map__delegate;
  return hash_map
}();
cljs.core.array_map = function() {
  var array_map__delegate = function(keyvals) {
    return new cljs.core.PersistentArrayMap(null, cljs.core.quot.call(null, cljs.core.count.call(null, keyvals), 2), cljs.core.apply.call(null, cljs.core.array, keyvals), null)
  };
  var array_map = function(var_args) {
    var keyvals = null;
    if(goog.isDef(var_args)) {
      keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return array_map__delegate.call(this, keyvals)
  };
  array_map.cljs$lang$maxFixedArity = 0;
  array_map.cljs$lang$applyTo = function(arglist__19485) {
    var keyvals = cljs.core.seq(arglist__19485);
    return array_map__delegate(keyvals)
  };
  array_map.cljs$lang$arity$variadic = array_map__delegate;
  return array_map
}();
cljs.core.obj_map = function() {
  var obj_map__delegate = function(keyvals) {
    var ks__19489 = [];
    var obj__19490 = {};
    var kvs__19491 = cljs.core.seq.call(null, keyvals);
    while(true) {
      if(kvs__19491) {
        ks__19489.push(cljs.core.first.call(null, kvs__19491));
        obj__19490[cljs.core.first.call(null, kvs__19491)] = cljs.core.second.call(null, kvs__19491);
        var G__19492 = cljs.core.nnext.call(null, kvs__19491);
        kvs__19491 = G__19492;
        continue
      }else {
        return cljs.core.ObjMap.fromObject.call(null, ks__19489, obj__19490)
      }
      break
    }
  };
  var obj_map = function(var_args) {
    var keyvals = null;
    if(goog.isDef(var_args)) {
      keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return obj_map__delegate.call(this, keyvals)
  };
  obj_map.cljs$lang$maxFixedArity = 0;
  obj_map.cljs$lang$applyTo = function(arglist__19493) {
    var keyvals = cljs.core.seq(arglist__19493);
    return obj_map__delegate(keyvals)
  };
  obj_map.cljs$lang$arity$variadic = obj_map__delegate;
  return obj_map
}();
cljs.core.sorted_map = function() {
  var sorted_map__delegate = function(keyvals) {
    var in__19496 = cljs.core.seq.call(null, keyvals);
    var out__19497 = cljs.core.PersistentTreeMap.EMPTY;
    while(true) {
      if(in__19496) {
        var G__19498 = cljs.core.nnext.call(null, in__19496);
        var G__19499 = cljs.core.assoc.call(null, out__19497, cljs.core.first.call(null, in__19496), cljs.core.second.call(null, in__19496));
        in__19496 = G__19498;
        out__19497 = G__19499;
        continue
      }else {
        return out__19497
      }
      break
    }
  };
  var sorted_map = function(var_args) {
    var keyvals = null;
    if(goog.isDef(var_args)) {
      keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return sorted_map__delegate.call(this, keyvals)
  };
  sorted_map.cljs$lang$maxFixedArity = 0;
  sorted_map.cljs$lang$applyTo = function(arglist__19500) {
    var keyvals = cljs.core.seq(arglist__19500);
    return sorted_map__delegate(keyvals)
  };
  sorted_map.cljs$lang$arity$variadic = sorted_map__delegate;
  return sorted_map
}();
cljs.core.sorted_map_by = function() {
  var sorted_map_by__delegate = function(comparator, keyvals) {
    var in__19503 = cljs.core.seq.call(null, keyvals);
    var out__19504 = new cljs.core.PersistentTreeMap(comparator, null, 0, null, 0);
    while(true) {
      if(in__19503) {
        var G__19505 = cljs.core.nnext.call(null, in__19503);
        var G__19506 = cljs.core.assoc.call(null, out__19504, cljs.core.first.call(null, in__19503), cljs.core.second.call(null, in__19503));
        in__19503 = G__19505;
        out__19504 = G__19506;
        continue
      }else {
        return out__19504
      }
      break
    }
  };
  var sorted_map_by = function(comparator, var_args) {
    var keyvals = null;
    if(goog.isDef(var_args)) {
      keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return sorted_map_by__delegate.call(this, comparator, keyvals)
  };
  sorted_map_by.cljs$lang$maxFixedArity = 1;
  sorted_map_by.cljs$lang$applyTo = function(arglist__19507) {
    var comparator = cljs.core.first(arglist__19507);
    var keyvals = cljs.core.rest(arglist__19507);
    return sorted_map_by__delegate(comparator, keyvals)
  };
  sorted_map_by.cljs$lang$arity$variadic = sorted_map_by__delegate;
  return sorted_map_by
}();
cljs.core.keys = function keys(hash_map) {
  return cljs.core.seq.call(null, cljs.core.map.call(null, cljs.core.first, hash_map))
};
cljs.core.key = function key(map_entry) {
  return cljs.core._key.call(null, map_entry)
};
cljs.core.vals = function vals(hash_map) {
  return cljs.core.seq.call(null, cljs.core.map.call(null, cljs.core.second, hash_map))
};
cljs.core.val = function val(map_entry) {
  return cljs.core._val.call(null, map_entry)
};
cljs.core.merge = function() {
  var merge__delegate = function(maps) {
    if(cljs.core.truth_(cljs.core.some.call(null, cljs.core.identity, maps))) {
      return cljs.core.reduce.call(null, function(p1__19508_SHARP_, p2__19509_SHARP_) {
        return cljs.core.conj.call(null, function() {
          var or__3824__auto____19511 = p1__19508_SHARP_;
          if(cljs.core.truth_(or__3824__auto____19511)) {
            return or__3824__auto____19511
          }else {
            return cljs.core.ObjMap.EMPTY
          }
        }(), p2__19509_SHARP_)
      }, maps)
    }else {
      return null
    }
  };
  var merge = function(var_args) {
    var maps = null;
    if(goog.isDef(var_args)) {
      maps = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return merge__delegate.call(this, maps)
  };
  merge.cljs$lang$maxFixedArity = 0;
  merge.cljs$lang$applyTo = function(arglist__19512) {
    var maps = cljs.core.seq(arglist__19512);
    return merge__delegate(maps)
  };
  merge.cljs$lang$arity$variadic = merge__delegate;
  return merge
}();
cljs.core.merge_with = function() {
  var merge_with__delegate = function(f, maps) {
    if(cljs.core.truth_(cljs.core.some.call(null, cljs.core.identity, maps))) {
      var merge_entry__19520 = function(m, e) {
        var k__19518 = cljs.core.first.call(null, e);
        var v__19519 = cljs.core.second.call(null, e);
        if(cljs.core.contains_QMARK_.call(null, m, k__19518)) {
          return cljs.core.assoc.call(null, m, k__19518, f.call(null, cljs.core._lookup.call(null, m, k__19518, null), v__19519))
        }else {
          return cljs.core.assoc.call(null, m, k__19518, v__19519)
        }
      };
      var merge2__19522 = function(m1, m2) {
        return cljs.core.reduce.call(null, merge_entry__19520, function() {
          var or__3824__auto____19521 = m1;
          if(cljs.core.truth_(or__3824__auto____19521)) {
            return or__3824__auto____19521
          }else {
            return cljs.core.ObjMap.EMPTY
          }
        }(), cljs.core.seq.call(null, m2))
      };
      return cljs.core.reduce.call(null, merge2__19522, maps)
    }else {
      return null
    }
  };
  var merge_with = function(f, var_args) {
    var maps = null;
    if(goog.isDef(var_args)) {
      maps = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return merge_with__delegate.call(this, f, maps)
  };
  merge_with.cljs$lang$maxFixedArity = 1;
  merge_with.cljs$lang$applyTo = function(arglist__19523) {
    var f = cljs.core.first(arglist__19523);
    var maps = cljs.core.rest(arglist__19523);
    return merge_with__delegate(f, maps)
  };
  merge_with.cljs$lang$arity$variadic = merge_with__delegate;
  return merge_with
}();
cljs.core.select_keys = function select_keys(map, keyseq) {
  var ret__19528 = cljs.core.ObjMap.EMPTY;
  var keys__19529 = cljs.core.seq.call(null, keyseq);
  while(true) {
    if(keys__19529) {
      var key__19530 = cljs.core.first.call(null, keys__19529);
      var entry__19531 = cljs.core._lookup.call(null, map, key__19530, "\ufdd0'cljs.core/not-found");
      var G__19532 = cljs.core.not_EQ_.call(null, entry__19531, "\ufdd0'cljs.core/not-found") ? cljs.core.assoc.call(null, ret__19528, key__19530, entry__19531) : ret__19528;
      var G__19533 = cljs.core.next.call(null, keys__19529);
      ret__19528 = G__19532;
      keys__19529 = G__19533;
      continue
    }else {
      return ret__19528
    }
    break
  }
};
goog.provide("cljs.core.PersistentHashSet");
cljs.core.PersistentHashSet = function(meta, hash_map, __hash) {
  this.meta = meta;
  this.hash_map = hash_map;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 4;
  this.cljs$lang$protocol_mask$partition0$ = 15077647
};
cljs.core.PersistentHashSet.cljs$lang$type = true;
cljs.core.PersistentHashSet.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentHashSet")
};
cljs.core.PersistentHashSet.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentHashSet")
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEditableCollection$_as_transient$arity$1 = function(coll) {
  var this__19537 = this;
  return new cljs.core.TransientHashSet(cljs.core.transient$.call(null, this__19537.hash_map))
};
cljs.core.PersistentHashSet.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19538 = this;
  var h__2247__auto____19539 = this__19538.__hash;
  if(!(h__2247__auto____19539 == null)) {
    return h__2247__auto____19539
  }else {
    var h__2247__auto____19540 = cljs.core.hash_iset.call(null, coll);
    this__19538.__hash = h__2247__auto____19540;
    return h__2247__auto____19540
  }
};
cljs.core.PersistentHashSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, v) {
  var this__19541 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, v, null)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, v, not_found) {
  var this__19542 = this;
  if(cljs.core.truth_(cljs.core._contains_key_QMARK_.call(null, this__19542.hash_map, v))) {
    return v
  }else {
    return not_found
  }
};
cljs.core.PersistentHashSet.prototype.call = function() {
  var G__19563 = null;
  var G__19563__2 = function(this_sym19543, k) {
    var this__19545 = this;
    var this_sym19543__19546 = this;
    var coll__19547 = this_sym19543__19546;
    return coll__19547.cljs$core$ILookup$_lookup$arity$2(coll__19547, k)
  };
  var G__19563__3 = function(this_sym19544, k, not_found) {
    var this__19545 = this;
    var this_sym19544__19548 = this;
    var coll__19549 = this_sym19544__19548;
    return coll__19549.cljs$core$ILookup$_lookup$arity$3(coll__19549, k, not_found)
  };
  G__19563 = function(this_sym19544, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19563__2.call(this, this_sym19544, k);
      case 3:
        return G__19563__3.call(this, this_sym19544, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19563
}();
cljs.core.PersistentHashSet.prototype.apply = function(this_sym19535, args19536) {
  var this__19550 = this;
  return this_sym19535.call.apply(this_sym19535, [this_sym19535].concat(args19536.slice()))
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19551 = this;
  return new cljs.core.PersistentHashSet(this__19551.meta, cljs.core.assoc.call(null, this__19551.hash_map, o, null), null)
};
cljs.core.PersistentHashSet.prototype.toString = function() {
  var this__19552 = this;
  var this__19553 = this;
  return cljs.core.pr_str.call(null, this__19553)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19554 = this;
  return cljs.core.keys.call(null, this__19554.hash_map)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ISet$_disjoin$arity$2 = function(coll, v) {
  var this__19555 = this;
  return new cljs.core.PersistentHashSet(this__19555.meta, cljs.core.dissoc.call(null, this__19555.hash_map, v), null)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19556 = this;
  return cljs.core.count.call(null, cljs.core.seq.call(null, coll))
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19557 = this;
  var and__3822__auto____19558 = cljs.core.set_QMARK_.call(null, other);
  if(and__3822__auto____19558) {
    var and__3822__auto____19559 = cljs.core.count.call(null, coll) === cljs.core.count.call(null, other);
    if(and__3822__auto____19559) {
      return cljs.core.every_QMARK_.call(null, function(p1__19534_SHARP_) {
        return cljs.core.contains_QMARK_.call(null, coll, p1__19534_SHARP_)
      }, other)
    }else {
      return and__3822__auto____19559
    }
  }else {
    return and__3822__auto____19558
  }
};
cljs.core.PersistentHashSet.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19560 = this;
  return new cljs.core.PersistentHashSet(meta, this__19560.hash_map, this__19560.__hash)
};
cljs.core.PersistentHashSet.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19561 = this;
  return this__19561.meta
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19562 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentHashSet.EMPTY, this__19562.meta)
};
cljs.core.PersistentHashSet;
cljs.core.PersistentHashSet.EMPTY = new cljs.core.PersistentHashSet(null, cljs.core.hash_map.call(null), 0);
cljs.core.PersistentHashSet.fromArray = function(items) {
  var len__19564 = cljs.core.count.call(null, items);
  var i__19565 = 0;
  var out__19566 = cljs.core.transient$.call(null, cljs.core.PersistentHashSet.EMPTY);
  while(true) {
    if(i__19565 < len__19564) {
      var G__19567 = i__19565 + 1;
      var G__19568 = cljs.core.conj_BANG_.call(null, out__19566, items[i__19565]);
      i__19565 = G__19567;
      out__19566 = G__19568;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, out__19566)
    }
    break
  }
};
goog.provide("cljs.core.TransientHashSet");
cljs.core.TransientHashSet = function(transient_map) {
  this.transient_map = transient_map;
  this.cljs$lang$protocol_mask$partition0$ = 259;
  this.cljs$lang$protocol_mask$partition1$ = 136
};
cljs.core.TransientHashSet.cljs$lang$type = true;
cljs.core.TransientHashSet.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/TransientHashSet")
};
cljs.core.TransientHashSet.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/TransientHashSet")
};
cljs.core.TransientHashSet.prototype.call = function() {
  var G__19586 = null;
  var G__19586__2 = function(this_sym19572, k) {
    var this__19574 = this;
    var this_sym19572__19575 = this;
    var tcoll__19576 = this_sym19572__19575;
    if(cljs.core._lookup.call(null, this__19574.transient_map, k, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
      return null
    }else {
      return k
    }
  };
  var G__19586__3 = function(this_sym19573, k, not_found) {
    var this__19574 = this;
    var this_sym19573__19577 = this;
    var tcoll__19578 = this_sym19573__19577;
    if(cljs.core._lookup.call(null, this__19574.transient_map, k, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
      return not_found
    }else {
      return k
    }
  };
  G__19586 = function(this_sym19573, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19586__2.call(this, this_sym19573, k);
      case 3:
        return G__19586__3.call(this, this_sym19573, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19586
}();
cljs.core.TransientHashSet.prototype.apply = function(this_sym19570, args19571) {
  var this__19579 = this;
  return this_sym19570.call.apply(this_sym19570, [this_sym19570].concat(args19571.slice()))
};
cljs.core.TransientHashSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(tcoll, v) {
  var this__19580 = this;
  return tcoll.cljs$core$ILookup$_lookup$arity$3(tcoll, v, null)
};
cljs.core.TransientHashSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(tcoll, v, not_found) {
  var this__19581 = this;
  if(cljs.core._lookup.call(null, this__19581.transient_map, v, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
    return not_found
  }else {
    return v
  }
};
cljs.core.TransientHashSet.prototype.cljs$core$ICounted$_count$arity$1 = function(tcoll) {
  var this__19582 = this;
  return cljs.core.count.call(null, this__19582.transient_map)
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientSet$_disjoin_BANG_$arity$2 = function(tcoll, v) {
  var this__19583 = this;
  this__19583.transient_map = cljs.core.dissoc_BANG_.call(null, this__19583.transient_map, v);
  return tcoll
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, o) {
  var this__19584 = this;
  this__19584.transient_map = cljs.core.assoc_BANG_.call(null, this__19584.transient_map, o, null);
  return tcoll
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__19585 = this;
  return new cljs.core.PersistentHashSet(null, cljs.core.persistent_BANG_.call(null, this__19585.transient_map), null)
};
cljs.core.TransientHashSet;
goog.provide("cljs.core.PersistentTreeSet");
cljs.core.PersistentTreeSet = function(meta, tree_map, __hash) {
  this.meta = meta;
  this.tree_map = tree_map;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 417730831
};
cljs.core.PersistentTreeSet.cljs$lang$type = true;
cljs.core.PersistentTreeSet.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/PersistentTreeSet")
};
cljs.core.PersistentTreeSet.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/PersistentTreeSet")
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19589 = this;
  var h__2247__auto____19590 = this__19589.__hash;
  if(!(h__2247__auto____19590 == null)) {
    return h__2247__auto____19590
  }else {
    var h__2247__auto____19591 = cljs.core.hash_iset.call(null, coll);
    this__19589.__hash = h__2247__auto____19591;
    return h__2247__auto____19591
  }
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, v) {
  var this__19592 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, v, null)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, v, not_found) {
  var this__19593 = this;
  if(cljs.core.truth_(cljs.core._contains_key_QMARK_.call(null, this__19593.tree_map, v))) {
    return v
  }else {
    return not_found
  }
};
cljs.core.PersistentTreeSet.prototype.call = function() {
  var G__19619 = null;
  var G__19619__2 = function(this_sym19594, k) {
    var this__19596 = this;
    var this_sym19594__19597 = this;
    var coll__19598 = this_sym19594__19597;
    return coll__19598.cljs$core$ILookup$_lookup$arity$2(coll__19598, k)
  };
  var G__19619__3 = function(this_sym19595, k, not_found) {
    var this__19596 = this;
    var this_sym19595__19599 = this;
    var coll__19600 = this_sym19595__19599;
    return coll__19600.cljs$core$ILookup$_lookup$arity$3(coll__19600, k, not_found)
  };
  G__19619 = function(this_sym19595, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19619__2.call(this, this_sym19595, k);
      case 3:
        return G__19619__3.call(this, this_sym19595, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19619
}();
cljs.core.PersistentTreeSet.prototype.apply = function(this_sym19587, args19588) {
  var this__19601 = this;
  return this_sym19587.call.apply(this_sym19587, [this_sym19587].concat(args19588.slice()))
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19602 = this;
  return new cljs.core.PersistentTreeSet(this__19602.meta, cljs.core.assoc.call(null, this__19602.tree_map, o, null), null)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__19603 = this;
  return cljs.core.map.call(null, cljs.core.key, cljs.core.rseq.call(null, this__19603.tree_map))
};
cljs.core.PersistentTreeSet.prototype.toString = function() {
  var this__19604 = this;
  var this__19605 = this;
  return cljs.core.pr_str.call(null, this__19605)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_sorted_seq$arity$2 = function(coll, ascending_QMARK_) {
  var this__19606 = this;
  return cljs.core.map.call(null, cljs.core.key, cljs.core._sorted_seq.call(null, this__19606.tree_map, ascending_QMARK_))
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_sorted_seq_from$arity$3 = function(coll, k, ascending_QMARK_) {
  var this__19607 = this;
  return cljs.core.map.call(null, cljs.core.key, cljs.core._sorted_seq_from.call(null, this__19607.tree_map, k, ascending_QMARK_))
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_entry_key$arity$2 = function(coll, entry) {
  var this__19608 = this;
  return entry
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_comparator$arity$1 = function(coll) {
  var this__19609 = this;
  return cljs.core._comparator.call(null, this__19609.tree_map)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19610 = this;
  return cljs.core.keys.call(null, this__19610.tree_map)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISet$_disjoin$arity$2 = function(coll, v) {
  var this__19611 = this;
  return new cljs.core.PersistentTreeSet(this__19611.meta, cljs.core.dissoc.call(null, this__19611.tree_map, v), null)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19612 = this;
  return cljs.core.count.call(null, this__19612.tree_map)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19613 = this;
  var and__3822__auto____19614 = cljs.core.set_QMARK_.call(null, other);
  if(and__3822__auto____19614) {
    var and__3822__auto____19615 = cljs.core.count.call(null, coll) === cljs.core.count.call(null, other);
    if(and__3822__auto____19615) {
      return cljs.core.every_QMARK_.call(null, function(p1__19569_SHARP_) {
        return cljs.core.contains_QMARK_.call(null, coll, p1__19569_SHARP_)
      }, other)
    }else {
      return and__3822__auto____19615
    }
  }else {
    return and__3822__auto____19614
  }
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19616 = this;
  return new cljs.core.PersistentTreeSet(meta, this__19616.tree_map, this__19616.__hash)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19617 = this;
  return this__19617.meta
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19618 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentTreeSet.EMPTY, this__19618.meta)
};
cljs.core.PersistentTreeSet;
cljs.core.PersistentTreeSet.EMPTY = new cljs.core.PersistentTreeSet(null, cljs.core.sorted_map.call(null), 0);
cljs.core.hash_set = function() {
  var hash_set = null;
  var hash_set__0 = function() {
    return cljs.core.PersistentHashSet.EMPTY
  };
  var hash_set__1 = function() {
    var G__19624__delegate = function(keys) {
      var in__19622 = cljs.core.seq.call(null, keys);
      var out__19623 = cljs.core.transient$.call(null, cljs.core.PersistentHashSet.EMPTY);
      while(true) {
        if(cljs.core.seq.call(null, in__19622)) {
          var G__19625 = cljs.core.next.call(null, in__19622);
          var G__19626 = cljs.core.conj_BANG_.call(null, out__19623, cljs.core.first.call(null, in__19622));
          in__19622 = G__19625;
          out__19623 = G__19626;
          continue
        }else {
          return cljs.core.persistent_BANG_.call(null, out__19623)
        }
        break
      }
    };
    var G__19624 = function(var_args) {
      var keys = null;
      if(goog.isDef(var_args)) {
        keys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__19624__delegate.call(this, keys)
    };
    G__19624.cljs$lang$maxFixedArity = 0;
    G__19624.cljs$lang$applyTo = function(arglist__19627) {
      var keys = cljs.core.seq(arglist__19627);
      return G__19624__delegate(keys)
    };
    G__19624.cljs$lang$arity$variadic = G__19624__delegate;
    return G__19624
  }();
  hash_set = function(var_args) {
    var keys = var_args;
    switch(arguments.length) {
      case 0:
        return hash_set__0.call(this);
      default:
        return hash_set__1.cljs$lang$arity$variadic(cljs.core.array_seq(arguments, 0))
    }
    throw"Invalid arity: " + arguments.length;
  };
  hash_set.cljs$lang$maxFixedArity = 0;
  hash_set.cljs$lang$applyTo = hash_set__1.cljs$lang$applyTo;
  hash_set.cljs$lang$arity$0 = hash_set__0;
  hash_set.cljs$lang$arity$variadic = hash_set__1.cljs$lang$arity$variadic;
  return hash_set
}();
cljs.core.set = function set(coll) {
  return cljs.core.apply.call(null, cljs.core.hash_set, coll)
};
cljs.core.sorted_set = function() {
  var sorted_set__delegate = function(keys) {
    return cljs.core.reduce.call(null, cljs.core._conj, cljs.core.PersistentTreeSet.EMPTY, keys)
  };
  var sorted_set = function(var_args) {
    var keys = null;
    if(goog.isDef(var_args)) {
      keys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return sorted_set__delegate.call(this, keys)
  };
  sorted_set.cljs$lang$maxFixedArity = 0;
  sorted_set.cljs$lang$applyTo = function(arglist__19628) {
    var keys = cljs.core.seq(arglist__19628);
    return sorted_set__delegate(keys)
  };
  sorted_set.cljs$lang$arity$variadic = sorted_set__delegate;
  return sorted_set
}();
cljs.core.sorted_set_by = function() {
  var sorted_set_by__delegate = function(comparator, keys) {
    return cljs.core.reduce.call(null, cljs.core._conj, new cljs.core.PersistentTreeSet(null, cljs.core.sorted_map_by.call(null, comparator), 0), keys)
  };
  var sorted_set_by = function(comparator, var_args) {
    var keys = null;
    if(goog.isDef(var_args)) {
      keys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return sorted_set_by__delegate.call(this, comparator, keys)
  };
  sorted_set_by.cljs$lang$maxFixedArity = 1;
  sorted_set_by.cljs$lang$applyTo = function(arglist__19630) {
    var comparator = cljs.core.first(arglist__19630);
    var keys = cljs.core.rest(arglist__19630);
    return sorted_set_by__delegate(comparator, keys)
  };
  sorted_set_by.cljs$lang$arity$variadic = sorted_set_by__delegate;
  return sorted_set_by
}();
cljs.core.replace = function replace(smap, coll) {
  if(cljs.core.vector_QMARK_.call(null, coll)) {
    var n__19636 = cljs.core.count.call(null, coll);
    return cljs.core.reduce.call(null, function(v, i) {
      var temp__3971__auto____19637 = cljs.core.find.call(null, smap, cljs.core.nth.call(null, v, i));
      if(cljs.core.truth_(temp__3971__auto____19637)) {
        var e__19638 = temp__3971__auto____19637;
        return cljs.core.assoc.call(null, v, i, cljs.core.second.call(null, e__19638))
      }else {
        return v
      }
    }, coll, cljs.core.take.call(null, n__19636, cljs.core.iterate.call(null, cljs.core.inc, 0)))
  }else {
    return cljs.core.map.call(null, function(p1__19629_SHARP_) {
      var temp__3971__auto____19639 = cljs.core.find.call(null, smap, p1__19629_SHARP_);
      if(cljs.core.truth_(temp__3971__auto____19639)) {
        var e__19640 = temp__3971__auto____19639;
        return cljs.core.second.call(null, e__19640)
      }else {
        return p1__19629_SHARP_
      }
    }, coll)
  }
};
cljs.core.distinct = function distinct(coll) {
  var step__19670 = function step(xs, seen) {
    return new cljs.core.LazySeq(null, false, function() {
      return function(p__19663, seen) {
        while(true) {
          var vec__19664__19665 = p__19663;
          var f__19666 = cljs.core.nth.call(null, vec__19664__19665, 0, null);
          var xs__19667 = vec__19664__19665;
          var temp__3974__auto____19668 = cljs.core.seq.call(null, xs__19667);
          if(temp__3974__auto____19668) {
            var s__19669 = temp__3974__auto____19668;
            if(cljs.core.contains_QMARK_.call(null, seen, f__19666)) {
              var G__19671 = cljs.core.rest.call(null, s__19669);
              var G__19672 = seen;
              p__19663 = G__19671;
              seen = G__19672;
              continue
            }else {
              return cljs.core.cons.call(null, f__19666, step.call(null, cljs.core.rest.call(null, s__19669), cljs.core.conj.call(null, seen, f__19666)))
            }
          }else {
            return null
          }
          break
        }
      }.call(null, xs, seen)
    }, null)
  };
  return step__19670.call(null, coll, cljs.core.PersistentHashSet.EMPTY)
};
cljs.core.butlast = function butlast(s) {
  var ret__19675 = cljs.core.PersistentVector.EMPTY;
  var s__19676 = s;
  while(true) {
    if(cljs.core.next.call(null, s__19676)) {
      var G__19677 = cljs.core.conj.call(null, ret__19675, cljs.core.first.call(null, s__19676));
      var G__19678 = cljs.core.next.call(null, s__19676);
      ret__19675 = G__19677;
      s__19676 = G__19678;
      continue
    }else {
      return cljs.core.seq.call(null, ret__19675)
    }
    break
  }
};
cljs.core.name = function name(x) {
  if(cljs.core.string_QMARK_.call(null, x)) {
    return x
  }else {
    if(function() {
      var or__3824__auto____19681 = cljs.core.keyword_QMARK_.call(null, x);
      if(or__3824__auto____19681) {
        return or__3824__auto____19681
      }else {
        return cljs.core.symbol_QMARK_.call(null, x)
      }
    }()) {
      var i__19682 = x.lastIndexOf("/");
      if(i__19682 < 0) {
        return cljs.core.subs.call(null, x, 2)
      }else {
        return cljs.core.subs.call(null, x, i__19682 + 1)
      }
    }else {
      if("\ufdd0'else") {
        throw new Error([cljs.core.str("Doesn't support name: "), cljs.core.str(x)].join(""));
      }else {
        return null
      }
    }
  }
};
cljs.core.namespace = function namespace(x) {
  if(function() {
    var or__3824__auto____19685 = cljs.core.keyword_QMARK_.call(null, x);
    if(or__3824__auto____19685) {
      return or__3824__auto____19685
    }else {
      return cljs.core.symbol_QMARK_.call(null, x)
    }
  }()) {
    var i__19686 = x.lastIndexOf("/");
    if(i__19686 > -1) {
      return cljs.core.subs.call(null, x, 2, i__19686)
    }else {
      return null
    }
  }else {
    throw new Error([cljs.core.str("Doesn't support namespace: "), cljs.core.str(x)].join(""));
  }
};
cljs.core.zipmap = function zipmap(keys, vals) {
  var map__19693 = cljs.core.ObjMap.EMPTY;
  var ks__19694 = cljs.core.seq.call(null, keys);
  var vs__19695 = cljs.core.seq.call(null, vals);
  while(true) {
    if(function() {
      var and__3822__auto____19696 = ks__19694;
      if(and__3822__auto____19696) {
        return vs__19695
      }else {
        return and__3822__auto____19696
      }
    }()) {
      var G__19697 = cljs.core.assoc.call(null, map__19693, cljs.core.first.call(null, ks__19694), cljs.core.first.call(null, vs__19695));
      var G__19698 = cljs.core.next.call(null, ks__19694);
      var G__19699 = cljs.core.next.call(null, vs__19695);
      map__19693 = G__19697;
      ks__19694 = G__19698;
      vs__19695 = G__19699;
      continue
    }else {
      return map__19693
    }
    break
  }
};
cljs.core.max_key = function() {
  var max_key = null;
  var max_key__2 = function(k, x) {
    return x
  };
  var max_key__3 = function(k, x, y) {
    if(k.call(null, x) > k.call(null, y)) {
      return x
    }else {
      return y
    }
  };
  var max_key__4 = function() {
    var G__19702__delegate = function(k, x, y, more) {
      return cljs.core.reduce.call(null, function(p1__19687_SHARP_, p2__19688_SHARP_) {
        return max_key.call(null, k, p1__19687_SHARP_, p2__19688_SHARP_)
      }, max_key.call(null, k, x, y), more)
    };
    var G__19702 = function(k, x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__19702__delegate.call(this, k, x, y, more)
    };
    G__19702.cljs$lang$maxFixedArity = 3;
    G__19702.cljs$lang$applyTo = function(arglist__19703) {
      var k = cljs.core.first(arglist__19703);
      var x = cljs.core.first(cljs.core.next(arglist__19703));
      var y = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19703)));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19703)));
      return G__19702__delegate(k, x, y, more)
    };
    G__19702.cljs$lang$arity$variadic = G__19702__delegate;
    return G__19702
  }();
  max_key = function(k, x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 2:
        return max_key__2.call(this, k, x);
      case 3:
        return max_key__3.call(this, k, x, y);
      default:
        return max_key__4.cljs$lang$arity$variadic(k, x, y, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  max_key.cljs$lang$maxFixedArity = 3;
  max_key.cljs$lang$applyTo = max_key__4.cljs$lang$applyTo;
  max_key.cljs$lang$arity$2 = max_key__2;
  max_key.cljs$lang$arity$3 = max_key__3;
  max_key.cljs$lang$arity$variadic = max_key__4.cljs$lang$arity$variadic;
  return max_key
}();
cljs.core.min_key = function() {
  var min_key = null;
  var min_key__2 = function(k, x) {
    return x
  };
  var min_key__3 = function(k, x, y) {
    if(k.call(null, x) < k.call(null, y)) {
      return x
    }else {
      return y
    }
  };
  var min_key__4 = function() {
    var G__19704__delegate = function(k, x, y, more) {
      return cljs.core.reduce.call(null, function(p1__19700_SHARP_, p2__19701_SHARP_) {
        return min_key.call(null, k, p1__19700_SHARP_, p2__19701_SHARP_)
      }, min_key.call(null, k, x, y), more)
    };
    var G__19704 = function(k, x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__19704__delegate.call(this, k, x, y, more)
    };
    G__19704.cljs$lang$maxFixedArity = 3;
    G__19704.cljs$lang$applyTo = function(arglist__19705) {
      var k = cljs.core.first(arglist__19705);
      var x = cljs.core.first(cljs.core.next(arglist__19705));
      var y = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19705)));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19705)));
      return G__19704__delegate(k, x, y, more)
    };
    G__19704.cljs$lang$arity$variadic = G__19704__delegate;
    return G__19704
  }();
  min_key = function(k, x, y, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 2:
        return min_key__2.call(this, k, x);
      case 3:
        return min_key__3.call(this, k, x, y);
      default:
        return min_key__4.cljs$lang$arity$variadic(k, x, y, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  min_key.cljs$lang$maxFixedArity = 3;
  min_key.cljs$lang$applyTo = min_key__4.cljs$lang$applyTo;
  min_key.cljs$lang$arity$2 = min_key__2;
  min_key.cljs$lang$arity$3 = min_key__3;
  min_key.cljs$lang$arity$variadic = min_key__4.cljs$lang$arity$variadic;
  return min_key
}();
cljs.core.partition_all = function() {
  var partition_all = null;
  var partition_all__2 = function(n, coll) {
    return partition_all.call(null, n, n, coll)
  };
  var partition_all__3 = function(n, step, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____19708 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____19708) {
        var s__19709 = temp__3974__auto____19708;
        return cljs.core.cons.call(null, cljs.core.take.call(null, n, s__19709), partition_all.call(null, n, step, cljs.core.drop.call(null, step, s__19709)))
      }else {
        return null
      }
    }, null)
  };
  partition_all = function(n, step, coll) {
    switch(arguments.length) {
      case 2:
        return partition_all__2.call(this, n, step);
      case 3:
        return partition_all__3.call(this, n, step, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  partition_all.cljs$lang$arity$2 = partition_all__2;
  partition_all.cljs$lang$arity$3 = partition_all__3;
  return partition_all
}();
cljs.core.take_while = function take_while(pred, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____19712 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____19712) {
      var s__19713 = temp__3974__auto____19712;
      if(cljs.core.truth_(pred.call(null, cljs.core.first.call(null, s__19713)))) {
        return cljs.core.cons.call(null, cljs.core.first.call(null, s__19713), take_while.call(null, pred, cljs.core.rest.call(null, s__19713)))
      }else {
        return null
      }
    }else {
      return null
    }
  }, null)
};
cljs.core.mk_bound_fn = function mk_bound_fn(sc, test, key) {
  return function(e) {
    var comp__19715 = cljs.core._comparator.call(null, sc);
    return test.call(null, comp__19715.call(null, cljs.core._entry_key.call(null, sc, e), key), 0)
  }
};
cljs.core.subseq = function() {
  var subseq = null;
  var subseq__3 = function(sc, test, key) {
    var include__19727 = cljs.core.mk_bound_fn.call(null, sc, test, key);
    if(cljs.core.truth_(cljs.core.PersistentHashSet.fromArray([cljs.core._GT_, cljs.core._GT__EQ_]).call(null, test))) {
      var temp__3974__auto____19728 = cljs.core._sorted_seq_from.call(null, sc, key, true);
      if(cljs.core.truth_(temp__3974__auto____19728)) {
        var vec__19729__19730 = temp__3974__auto____19728;
        var e__19731 = cljs.core.nth.call(null, vec__19729__19730, 0, null);
        var s__19732 = vec__19729__19730;
        if(cljs.core.truth_(include__19727.call(null, e__19731))) {
          return s__19732
        }else {
          return cljs.core.next.call(null, s__19732)
        }
      }else {
        return null
      }
    }else {
      return cljs.core.take_while.call(null, include__19727, cljs.core._sorted_seq.call(null, sc, true))
    }
  };
  var subseq__5 = function(sc, start_test, start_key, end_test, end_key) {
    var temp__3974__auto____19733 = cljs.core._sorted_seq_from.call(null, sc, start_key, true);
    if(cljs.core.truth_(temp__3974__auto____19733)) {
      var vec__19734__19735 = temp__3974__auto____19733;
      var e__19736 = cljs.core.nth.call(null, vec__19734__19735, 0, null);
      var s__19737 = vec__19734__19735;
      return cljs.core.take_while.call(null, cljs.core.mk_bound_fn.call(null, sc, end_test, end_key), cljs.core.truth_(cljs.core.mk_bound_fn.call(null, sc, start_test, start_key).call(null, e__19736)) ? s__19737 : cljs.core.next.call(null, s__19737))
    }else {
      return null
    }
  };
  subseq = function(sc, start_test, start_key, end_test, end_key) {
    switch(arguments.length) {
      case 3:
        return subseq__3.call(this, sc, start_test, start_key);
      case 5:
        return subseq__5.call(this, sc, start_test, start_key, end_test, end_key)
    }
    throw"Invalid arity: " + arguments.length;
  };
  subseq.cljs$lang$arity$3 = subseq__3;
  subseq.cljs$lang$arity$5 = subseq__5;
  return subseq
}();
cljs.core.rsubseq = function() {
  var rsubseq = null;
  var rsubseq__3 = function(sc, test, key) {
    var include__19749 = cljs.core.mk_bound_fn.call(null, sc, test, key);
    if(cljs.core.truth_(cljs.core.PersistentHashSet.fromArray([cljs.core._LT_, cljs.core._LT__EQ_]).call(null, test))) {
      var temp__3974__auto____19750 = cljs.core._sorted_seq_from.call(null, sc, key, false);
      if(cljs.core.truth_(temp__3974__auto____19750)) {
        var vec__19751__19752 = temp__3974__auto____19750;
        var e__19753 = cljs.core.nth.call(null, vec__19751__19752, 0, null);
        var s__19754 = vec__19751__19752;
        if(cljs.core.truth_(include__19749.call(null, e__19753))) {
          return s__19754
        }else {
          return cljs.core.next.call(null, s__19754)
        }
      }else {
        return null
      }
    }else {
      return cljs.core.take_while.call(null, include__19749, cljs.core._sorted_seq.call(null, sc, false))
    }
  };
  var rsubseq__5 = function(sc, start_test, start_key, end_test, end_key) {
    var temp__3974__auto____19755 = cljs.core._sorted_seq_from.call(null, sc, end_key, false);
    if(cljs.core.truth_(temp__3974__auto____19755)) {
      var vec__19756__19757 = temp__3974__auto____19755;
      var e__19758 = cljs.core.nth.call(null, vec__19756__19757, 0, null);
      var s__19759 = vec__19756__19757;
      return cljs.core.take_while.call(null, cljs.core.mk_bound_fn.call(null, sc, start_test, start_key), cljs.core.truth_(cljs.core.mk_bound_fn.call(null, sc, end_test, end_key).call(null, e__19758)) ? s__19759 : cljs.core.next.call(null, s__19759))
    }else {
      return null
    }
  };
  rsubseq = function(sc, start_test, start_key, end_test, end_key) {
    switch(arguments.length) {
      case 3:
        return rsubseq__3.call(this, sc, start_test, start_key);
      case 5:
        return rsubseq__5.call(this, sc, start_test, start_key, end_test, end_key)
    }
    throw"Invalid arity: " + arguments.length;
  };
  rsubseq.cljs$lang$arity$3 = rsubseq__3;
  rsubseq.cljs$lang$arity$5 = rsubseq__5;
  return rsubseq
}();
goog.provide("cljs.core.Range");
cljs.core.Range = function(meta, start, end, step, __hash) {
  this.meta = meta;
  this.start = start;
  this.end = end;
  this.step = step;
  this.__hash = __hash;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 32375006
};
cljs.core.Range.cljs$lang$type = true;
cljs.core.Range.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Range")
};
cljs.core.Range.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Range")
};
cljs.core.Range.prototype.cljs$core$IHash$_hash$arity$1 = function(rng) {
  var this__19760 = this;
  var h__2247__auto____19761 = this__19760.__hash;
  if(!(h__2247__auto____19761 == null)) {
    return h__2247__auto____19761
  }else {
    var h__2247__auto____19762 = cljs.core.hash_coll.call(null, rng);
    this__19760.__hash = h__2247__auto____19762;
    return h__2247__auto____19762
  }
};
cljs.core.Range.prototype.cljs$core$INext$_next$arity$1 = function(rng) {
  var this__19763 = this;
  if(this__19763.step > 0) {
    if(this__19763.start + this__19763.step < this__19763.end) {
      return new cljs.core.Range(this__19763.meta, this__19763.start + this__19763.step, this__19763.end, this__19763.step, null)
    }else {
      return null
    }
  }else {
    if(this__19763.start + this__19763.step > this__19763.end) {
      return new cljs.core.Range(this__19763.meta, this__19763.start + this__19763.step, this__19763.end, this__19763.step, null)
    }else {
      return null
    }
  }
};
cljs.core.Range.prototype.cljs$core$ICollection$_conj$arity$2 = function(rng, o) {
  var this__19764 = this;
  return cljs.core.cons.call(null, o, rng)
};
cljs.core.Range.prototype.toString = function() {
  var this__19765 = this;
  var this__19766 = this;
  return cljs.core.pr_str.call(null, this__19766)
};
cljs.core.Range.prototype.cljs$core$IReduce$_reduce$arity$2 = function(rng, f) {
  var this__19767 = this;
  return cljs.core.ci_reduce.call(null, rng, f)
};
cljs.core.Range.prototype.cljs$core$IReduce$_reduce$arity$3 = function(rng, f, s) {
  var this__19768 = this;
  return cljs.core.ci_reduce.call(null, rng, f, s)
};
cljs.core.Range.prototype.cljs$core$ISeqable$_seq$arity$1 = function(rng) {
  var this__19769 = this;
  if(this__19769.step > 0) {
    if(this__19769.start < this__19769.end) {
      return rng
    }else {
      return null
    }
  }else {
    if(this__19769.start > this__19769.end) {
      return rng
    }else {
      return null
    }
  }
};
cljs.core.Range.prototype.cljs$core$ICounted$_count$arity$1 = function(rng) {
  var this__19770 = this;
  if(cljs.core.not.call(null, rng.cljs$core$ISeqable$_seq$arity$1(rng))) {
    return 0
  }else {
    return Math.ceil((this__19770.end - this__19770.start) / this__19770.step)
  }
};
cljs.core.Range.prototype.cljs$core$ISeq$_first$arity$1 = function(rng) {
  var this__19771 = this;
  return this__19771.start
};
cljs.core.Range.prototype.cljs$core$ISeq$_rest$arity$1 = function(rng) {
  var this__19772 = this;
  if(!(rng.cljs$core$ISeqable$_seq$arity$1(rng) == null)) {
    return new cljs.core.Range(this__19772.meta, this__19772.start + this__19772.step, this__19772.end, this__19772.step, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.Range.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(rng, other) {
  var this__19773 = this;
  return cljs.core.equiv_sequential.call(null, rng, other)
};
cljs.core.Range.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(rng, meta) {
  var this__19774 = this;
  return new cljs.core.Range(meta, this__19774.start, this__19774.end, this__19774.step, this__19774.__hash)
};
cljs.core.Range.prototype.cljs$core$IMeta$_meta$arity$1 = function(rng) {
  var this__19775 = this;
  return this__19775.meta
};
cljs.core.Range.prototype.cljs$core$IIndexed$_nth$arity$2 = function(rng, n) {
  var this__19776 = this;
  if(n < rng.cljs$core$ICounted$_count$arity$1(rng)) {
    return this__19776.start + n * this__19776.step
  }else {
    if(function() {
      var and__3822__auto____19777 = this__19776.start > this__19776.end;
      if(and__3822__auto____19777) {
        return this__19776.step === 0
      }else {
        return and__3822__auto____19777
      }
    }()) {
      return this__19776.start
    }else {
      throw new Error("Index out of bounds");
    }
  }
};
cljs.core.Range.prototype.cljs$core$IIndexed$_nth$arity$3 = function(rng, n, not_found) {
  var this__19778 = this;
  if(n < rng.cljs$core$ICounted$_count$arity$1(rng)) {
    return this__19778.start + n * this__19778.step
  }else {
    if(function() {
      var and__3822__auto____19779 = this__19778.start > this__19778.end;
      if(and__3822__auto____19779) {
        return this__19778.step === 0
      }else {
        return and__3822__auto____19779
      }
    }()) {
      return this__19778.start
    }else {
      return not_found
    }
  }
};
cljs.core.Range.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(rng) {
  var this__19780 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19780.meta)
};
cljs.core.Range;
cljs.core.range = function() {
  var range = null;
  var range__0 = function() {
    return range.call(null, 0, Number.MAX_VALUE, 1)
  };
  var range__1 = function(end) {
    return range.call(null, 0, end, 1)
  };
  var range__2 = function(start, end) {
    return range.call(null, start, end, 1)
  };
  var range__3 = function(start, end, step) {
    return new cljs.core.Range(null, start, end, step, null)
  };
  range = function(start, end, step) {
    switch(arguments.length) {
      case 0:
        return range__0.call(this);
      case 1:
        return range__1.call(this, start);
      case 2:
        return range__2.call(this, start, end);
      case 3:
        return range__3.call(this, start, end, step)
    }
    throw"Invalid arity: " + arguments.length;
  };
  range.cljs$lang$arity$0 = range__0;
  range.cljs$lang$arity$1 = range__1;
  range.cljs$lang$arity$2 = range__2;
  range.cljs$lang$arity$3 = range__3;
  return range
}();
cljs.core.take_nth = function take_nth(n, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____19783 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____19783) {
      var s__19784 = temp__3974__auto____19783;
      return cljs.core.cons.call(null, cljs.core.first.call(null, s__19784), take_nth.call(null, n, cljs.core.drop.call(null, n, s__19784)))
    }else {
      return null
    }
  }, null)
};
cljs.core.split_with = function split_with(pred, coll) {
  return cljs.core.PersistentVector.fromArray([cljs.core.take_while.call(null, pred, coll), cljs.core.drop_while.call(null, pred, coll)], true)
};
cljs.core.partition_by = function partition_by(f, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____19791 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____19791) {
      var s__19792 = temp__3974__auto____19791;
      var fst__19793 = cljs.core.first.call(null, s__19792);
      var fv__19794 = f.call(null, fst__19793);
      var run__19795 = cljs.core.cons.call(null, fst__19793, cljs.core.take_while.call(null, function(p1__19785_SHARP_) {
        return cljs.core._EQ_.call(null, fv__19794, f.call(null, p1__19785_SHARP_))
      }, cljs.core.next.call(null, s__19792)));
      return cljs.core.cons.call(null, run__19795, partition_by.call(null, f, cljs.core.seq.call(null, cljs.core.drop.call(null, cljs.core.count.call(null, run__19795), s__19792))))
    }else {
      return null
    }
  }, null)
};
cljs.core.frequencies = function frequencies(coll) {
  return cljs.core.persistent_BANG_.call(null, cljs.core.reduce.call(null, function(counts, x) {
    return cljs.core.assoc_BANG_.call(null, counts, x, cljs.core._lookup.call(null, counts, x, 0) + 1)
  }, cljs.core.transient$.call(null, cljs.core.ObjMap.EMPTY), coll))
};
cljs.core.reductions = function() {
  var reductions = null;
  var reductions__2 = function(f, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3971__auto____19810 = cljs.core.seq.call(null, coll);
      if(temp__3971__auto____19810) {
        var s__19811 = temp__3971__auto____19810;
        return reductions.call(null, f, cljs.core.first.call(null, s__19811), cljs.core.rest.call(null, s__19811))
      }else {
        return cljs.core.list.call(null, f.call(null))
      }
    }, null)
  };
  var reductions__3 = function(f, init, coll) {
    return cljs.core.cons.call(null, init, new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____19812 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____19812) {
        var s__19813 = temp__3974__auto____19812;
        return reductions.call(null, f, f.call(null, init, cljs.core.first.call(null, s__19813)), cljs.core.rest.call(null, s__19813))
      }else {
        return null
      }
    }, null))
  };
  reductions = function(f, init, coll) {
    switch(arguments.length) {
      case 2:
        return reductions__2.call(this, f, init);
      case 3:
        return reductions__3.call(this, f, init, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  reductions.cljs$lang$arity$2 = reductions__2;
  reductions.cljs$lang$arity$3 = reductions__3;
  return reductions
}();
cljs.core.juxt = function() {
  var juxt = null;
  var juxt__1 = function(f) {
    return function() {
      var G__19816 = null;
      var G__19816__0 = function() {
        return cljs.core.vector.call(null, f.call(null))
      };
      var G__19816__1 = function(x) {
        return cljs.core.vector.call(null, f.call(null, x))
      };
      var G__19816__2 = function(x, y) {
        return cljs.core.vector.call(null, f.call(null, x, y))
      };
      var G__19816__3 = function(x, y, z) {
        return cljs.core.vector.call(null, f.call(null, x, y, z))
      };
      var G__19816__4 = function() {
        var G__19817__delegate = function(x, y, z, args) {
          return cljs.core.vector.call(null, cljs.core.apply.call(null, f, x, y, z, args))
        };
        var G__19817 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__19817__delegate.call(this, x, y, z, args)
        };
        G__19817.cljs$lang$maxFixedArity = 3;
        G__19817.cljs$lang$applyTo = function(arglist__19818) {
          var x = cljs.core.first(arglist__19818);
          var y = cljs.core.first(cljs.core.next(arglist__19818));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19818)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19818)));
          return G__19817__delegate(x, y, z, args)
        };
        G__19817.cljs$lang$arity$variadic = G__19817__delegate;
        return G__19817
      }();
      G__19816 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__19816__0.call(this);
          case 1:
            return G__19816__1.call(this, x);
          case 2:
            return G__19816__2.call(this, x, y);
          case 3:
            return G__19816__3.call(this, x, y, z);
          default:
            return G__19816__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__19816.cljs$lang$maxFixedArity = 3;
      G__19816.cljs$lang$applyTo = G__19816__4.cljs$lang$applyTo;
      return G__19816
    }()
  };
  var juxt__2 = function(f, g) {
    return function() {
      var G__19819 = null;
      var G__19819__0 = function() {
        return cljs.core.vector.call(null, f.call(null), g.call(null))
      };
      var G__19819__1 = function(x) {
        return cljs.core.vector.call(null, f.call(null, x), g.call(null, x))
      };
      var G__19819__2 = function(x, y) {
        return cljs.core.vector.call(null, f.call(null, x, y), g.call(null, x, y))
      };
      var G__19819__3 = function(x, y, z) {
        return cljs.core.vector.call(null, f.call(null, x, y, z), g.call(null, x, y, z))
      };
      var G__19819__4 = function() {
        var G__19820__delegate = function(x, y, z, args) {
          return cljs.core.vector.call(null, cljs.core.apply.call(null, f, x, y, z, args), cljs.core.apply.call(null, g, x, y, z, args))
        };
        var G__19820 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__19820__delegate.call(this, x, y, z, args)
        };
        G__19820.cljs$lang$maxFixedArity = 3;
        G__19820.cljs$lang$applyTo = function(arglist__19821) {
          var x = cljs.core.first(arglist__19821);
          var y = cljs.core.first(cljs.core.next(arglist__19821));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19821)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19821)));
          return G__19820__delegate(x, y, z, args)
        };
        G__19820.cljs$lang$arity$variadic = G__19820__delegate;
        return G__19820
      }();
      G__19819 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__19819__0.call(this);
          case 1:
            return G__19819__1.call(this, x);
          case 2:
            return G__19819__2.call(this, x, y);
          case 3:
            return G__19819__3.call(this, x, y, z);
          default:
            return G__19819__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__19819.cljs$lang$maxFixedArity = 3;
      G__19819.cljs$lang$applyTo = G__19819__4.cljs$lang$applyTo;
      return G__19819
    }()
  };
  var juxt__3 = function(f, g, h) {
    return function() {
      var G__19822 = null;
      var G__19822__0 = function() {
        return cljs.core.vector.call(null, f.call(null), g.call(null), h.call(null))
      };
      var G__19822__1 = function(x) {
        return cljs.core.vector.call(null, f.call(null, x), g.call(null, x), h.call(null, x))
      };
      var G__19822__2 = function(x, y) {
        return cljs.core.vector.call(null, f.call(null, x, y), g.call(null, x, y), h.call(null, x, y))
      };
      var G__19822__3 = function(x, y, z) {
        return cljs.core.vector.call(null, f.call(null, x, y, z), g.call(null, x, y, z), h.call(null, x, y, z))
      };
      var G__19822__4 = function() {
        var G__19823__delegate = function(x, y, z, args) {
          return cljs.core.vector.call(null, cljs.core.apply.call(null, f, x, y, z, args), cljs.core.apply.call(null, g, x, y, z, args), cljs.core.apply.call(null, h, x, y, z, args))
        };
        var G__19823 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__19823__delegate.call(this, x, y, z, args)
        };
        G__19823.cljs$lang$maxFixedArity = 3;
        G__19823.cljs$lang$applyTo = function(arglist__19824) {
          var x = cljs.core.first(arglist__19824);
          var y = cljs.core.first(cljs.core.next(arglist__19824));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19824)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19824)));
          return G__19823__delegate(x, y, z, args)
        };
        G__19823.cljs$lang$arity$variadic = G__19823__delegate;
        return G__19823
      }();
      G__19822 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__19822__0.call(this);
          case 1:
            return G__19822__1.call(this, x);
          case 2:
            return G__19822__2.call(this, x, y);
          case 3:
            return G__19822__3.call(this, x, y, z);
          default:
            return G__19822__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__19822.cljs$lang$maxFixedArity = 3;
      G__19822.cljs$lang$applyTo = G__19822__4.cljs$lang$applyTo;
      return G__19822
    }()
  };
  var juxt__4 = function() {
    var G__19825__delegate = function(f, g, h, fs) {
      var fs__19815 = cljs.core.list_STAR_.call(null, f, g, h, fs);
      return function() {
        var G__19826 = null;
        var G__19826__0 = function() {
          return cljs.core.reduce.call(null, function(p1__19796_SHARP_, p2__19797_SHARP_) {
            return cljs.core.conj.call(null, p1__19796_SHARP_, p2__19797_SHARP_.call(null))
          }, cljs.core.PersistentVector.EMPTY, fs__19815)
        };
        var G__19826__1 = function(x) {
          return cljs.core.reduce.call(null, function(p1__19798_SHARP_, p2__19799_SHARP_) {
            return cljs.core.conj.call(null, p1__19798_SHARP_, p2__19799_SHARP_.call(null, x))
          }, cljs.core.PersistentVector.EMPTY, fs__19815)
        };
        var G__19826__2 = function(x, y) {
          return cljs.core.reduce.call(null, function(p1__19800_SHARP_, p2__19801_SHARP_) {
            return cljs.core.conj.call(null, p1__19800_SHARP_, p2__19801_SHARP_.call(null, x, y))
          }, cljs.core.PersistentVector.EMPTY, fs__19815)
        };
        var G__19826__3 = function(x, y, z) {
          return cljs.core.reduce.call(null, function(p1__19802_SHARP_, p2__19803_SHARP_) {
            return cljs.core.conj.call(null, p1__19802_SHARP_, p2__19803_SHARP_.call(null, x, y, z))
          }, cljs.core.PersistentVector.EMPTY, fs__19815)
        };
        var G__19826__4 = function() {
          var G__19827__delegate = function(x, y, z, args) {
            return cljs.core.reduce.call(null, function(p1__19804_SHARP_, p2__19805_SHARP_) {
              return cljs.core.conj.call(null, p1__19804_SHARP_, cljs.core.apply.call(null, p2__19805_SHARP_, x, y, z, args))
            }, cljs.core.PersistentVector.EMPTY, fs__19815)
          };
          var G__19827 = function(x, y, z, var_args) {
            var args = null;
            if(goog.isDef(var_args)) {
              args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
            }
            return G__19827__delegate.call(this, x, y, z, args)
          };
          G__19827.cljs$lang$maxFixedArity = 3;
          G__19827.cljs$lang$applyTo = function(arglist__19828) {
            var x = cljs.core.first(arglist__19828);
            var y = cljs.core.first(cljs.core.next(arglist__19828));
            var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19828)));
            var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19828)));
            return G__19827__delegate(x, y, z, args)
          };
          G__19827.cljs$lang$arity$variadic = G__19827__delegate;
          return G__19827
        }();
        G__19826 = function(x, y, z, var_args) {
          var args = var_args;
          switch(arguments.length) {
            case 0:
              return G__19826__0.call(this);
            case 1:
              return G__19826__1.call(this, x);
            case 2:
              return G__19826__2.call(this, x, y);
            case 3:
              return G__19826__3.call(this, x, y, z);
            default:
              return G__19826__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
          }
          throw"Invalid arity: " + arguments.length;
        };
        G__19826.cljs$lang$maxFixedArity = 3;
        G__19826.cljs$lang$applyTo = G__19826__4.cljs$lang$applyTo;
        return G__19826
      }()
    };
    var G__19825 = function(f, g, h, var_args) {
      var fs = null;
      if(goog.isDef(var_args)) {
        fs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__19825__delegate.call(this, f, g, h, fs)
    };
    G__19825.cljs$lang$maxFixedArity = 3;
    G__19825.cljs$lang$applyTo = function(arglist__19829) {
      var f = cljs.core.first(arglist__19829);
      var g = cljs.core.first(cljs.core.next(arglist__19829));
      var h = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19829)));
      var fs = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__19829)));
      return G__19825__delegate(f, g, h, fs)
    };
    G__19825.cljs$lang$arity$variadic = G__19825__delegate;
    return G__19825
  }();
  juxt = function(f, g, h, var_args) {
    var fs = var_args;
    switch(arguments.length) {
      case 1:
        return juxt__1.call(this, f);
      case 2:
        return juxt__2.call(this, f, g);
      case 3:
        return juxt__3.call(this, f, g, h);
      default:
        return juxt__4.cljs$lang$arity$variadic(f, g, h, cljs.core.array_seq(arguments, 3))
    }
    throw"Invalid arity: " + arguments.length;
  };
  juxt.cljs$lang$maxFixedArity = 3;
  juxt.cljs$lang$applyTo = juxt__4.cljs$lang$applyTo;
  juxt.cljs$lang$arity$1 = juxt__1;
  juxt.cljs$lang$arity$2 = juxt__2;
  juxt.cljs$lang$arity$3 = juxt__3;
  juxt.cljs$lang$arity$variadic = juxt__4.cljs$lang$arity$variadic;
  return juxt
}();
cljs.core.dorun = function() {
  var dorun = null;
  var dorun__1 = function(coll) {
    while(true) {
      if(cljs.core.seq.call(null, coll)) {
        var G__19832 = cljs.core.next.call(null, coll);
        coll = G__19832;
        continue
      }else {
        return null
      }
      break
    }
  };
  var dorun__2 = function(n, coll) {
    while(true) {
      if(cljs.core.truth_(function() {
        var and__3822__auto____19831 = cljs.core.seq.call(null, coll);
        if(and__3822__auto____19831) {
          return n > 0
        }else {
          return and__3822__auto____19831
        }
      }())) {
        var G__19833 = n - 1;
        var G__19834 = cljs.core.next.call(null, coll);
        n = G__19833;
        coll = G__19834;
        continue
      }else {
        return null
      }
      break
    }
  };
  dorun = function(n, coll) {
    switch(arguments.length) {
      case 1:
        return dorun__1.call(this, n);
      case 2:
        return dorun__2.call(this, n, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  dorun.cljs$lang$arity$1 = dorun__1;
  dorun.cljs$lang$arity$2 = dorun__2;
  return dorun
}();
cljs.core.doall = function() {
  var doall = null;
  var doall__1 = function(coll) {
    cljs.core.dorun.call(null, coll);
    return coll
  };
  var doall__2 = function(n, coll) {
    cljs.core.dorun.call(null, n, coll);
    return coll
  };
  doall = function(n, coll) {
    switch(arguments.length) {
      case 1:
        return doall__1.call(this, n);
      case 2:
        return doall__2.call(this, n, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  doall.cljs$lang$arity$1 = doall__1;
  doall.cljs$lang$arity$2 = doall__2;
  return doall
}();
cljs.core.regexp_QMARK_ = function regexp_QMARK_(o) {
  return o instanceof RegExp
};
cljs.core.re_matches = function re_matches(re, s) {
  var matches__19836 = re.exec(s);
  if(cljs.core._EQ_.call(null, cljs.core.first.call(null, matches__19836), s)) {
    if(cljs.core.count.call(null, matches__19836) === 1) {
      return cljs.core.first.call(null, matches__19836)
    }else {
      return cljs.core.vec.call(null, matches__19836)
    }
  }else {
    return null
  }
};
cljs.core.re_find = function re_find(re, s) {
  var matches__19838 = re.exec(s);
  if(matches__19838 == null) {
    return null
  }else {
    if(cljs.core.count.call(null, matches__19838) === 1) {
      return cljs.core.first.call(null, matches__19838)
    }else {
      return cljs.core.vec.call(null, matches__19838)
    }
  }
};
cljs.core.re_seq = function re_seq(re, s) {
  var match_data__19843 = cljs.core.re_find.call(null, re, s);
  var match_idx__19844 = s.search(re);
  var match_str__19845 = cljs.core.coll_QMARK_.call(null, match_data__19843) ? cljs.core.first.call(null, match_data__19843) : match_data__19843;
  var post_match__19846 = cljs.core.subs.call(null, s, match_idx__19844 + cljs.core.count.call(null, match_str__19845));
  if(cljs.core.truth_(match_data__19843)) {
    return new cljs.core.LazySeq(null, false, function() {
      return cljs.core.cons.call(null, match_data__19843, re_seq.call(null, re, post_match__19846))
    }, null)
  }else {
    return null
  }
};
cljs.core.re_pattern = function re_pattern(s) {
  var vec__19853__19854 = cljs.core.re_find.call(null, /^(?:\(\?([idmsux]*)\))?(.*)/, s);
  var ___19855 = cljs.core.nth.call(null, vec__19853__19854, 0, null);
  var flags__19856 = cljs.core.nth.call(null, vec__19853__19854, 1, null);
  var pattern__19857 = cljs.core.nth.call(null, vec__19853__19854, 2, null);
  return new RegExp(pattern__19857, flags__19856)
};
cljs.core.pr_sequential = function pr_sequential(print_one, begin, sep, end, opts, coll) {
  return cljs.core.concat.call(null, cljs.core.PersistentVector.fromArray([begin], true), cljs.core.flatten1.call(null, cljs.core.interpose.call(null, cljs.core.PersistentVector.fromArray([sep], true), cljs.core.map.call(null, function(p1__19847_SHARP_) {
    return print_one.call(null, p1__19847_SHARP_, opts)
  }, coll))), cljs.core.PersistentVector.fromArray([end], true))
};
cljs.core.pr_sequential_writer = function pr_sequential_writer(writer, print_one, begin, sep, end, opts, coll) {
  cljs.core._write.call(null, writer, begin);
  if(cljs.core.seq.call(null, coll)) {
    print_one.call(null, cljs.core.first.call(null, coll), writer, opts)
  }else {
  }
  var G__19861__19862 = cljs.core.seq.call(null, cljs.core.next.call(null, coll));
  while(true) {
    if(G__19861__19862) {
      var o__19863 = cljs.core.first.call(null, G__19861__19862);
      cljs.core._write.call(null, writer, sep);
      print_one.call(null, o__19863, writer, opts);
      var G__19864 = cljs.core.next.call(null, G__19861__19862);
      G__19861__19862 = G__19864;
      continue
    }else {
    }
    break
  }
  return cljs.core._write.call(null, writer, end)
};
cljs.core.write_all = function() {
  var write_all__delegate = function(writer, ss) {
    var G__19868__19869 = cljs.core.seq.call(null, ss);
    while(true) {
      if(G__19868__19869) {
        var s__19870 = cljs.core.first.call(null, G__19868__19869);
        cljs.core._write.call(null, writer, s__19870);
        var G__19871 = cljs.core.next.call(null, G__19868__19869);
        G__19868__19869 = G__19871;
        continue
      }else {
        return null
      }
      break
    }
  };
  var write_all = function(writer, var_args) {
    var ss = null;
    if(goog.isDef(var_args)) {
      ss = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return write_all__delegate.call(this, writer, ss)
  };
  write_all.cljs$lang$maxFixedArity = 1;
  write_all.cljs$lang$applyTo = function(arglist__19872) {
    var writer = cljs.core.first(arglist__19872);
    var ss = cljs.core.rest(arglist__19872);
    return write_all__delegate(writer, ss)
  };
  write_all.cljs$lang$arity$variadic = write_all__delegate;
  return write_all
}();
cljs.core.string_print = function string_print(x) {
  cljs.core._STAR_print_fn_STAR_.call(null, x);
  return null
};
cljs.core.flush = function flush() {
  return null
};
goog.provide("cljs.core.StringBufferWriter");
cljs.core.StringBufferWriter = function(sb) {
  this.sb = sb;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 1073741824
};
cljs.core.StringBufferWriter.cljs$lang$type = true;
cljs.core.StringBufferWriter.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/StringBufferWriter")
};
cljs.core.StringBufferWriter.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/StringBufferWriter")
};
cljs.core.StringBufferWriter.prototype.cljs$core$IWriter$_write$arity$2 = function(_, s) {
  var this__19873 = this;
  return this__19873.sb.append(s)
};
cljs.core.StringBufferWriter.prototype.cljs$core$IWriter$_flush$arity$1 = function(_) {
  var this__19874 = this;
  return null
};
cljs.core.StringBufferWriter;
cljs.core.pr_seq = function pr_seq(obj, opts) {
  if(obj == null) {
    return cljs.core.list.call(null, "nil")
  }else {
    if(void 0 === obj) {
      return cljs.core.list.call(null, "#<undefined>")
    }else {
      if("\ufdd0'else") {
        return cljs.core.concat.call(null, cljs.core.truth_(function() {
          var and__3822__auto____19884 = cljs.core._lookup.call(null, opts, "\ufdd0'meta", null);
          if(cljs.core.truth_(and__3822__auto____19884)) {
            var and__3822__auto____19888 = function() {
              var G__19885__19886 = obj;
              if(G__19885__19886) {
                if(function() {
                  var or__3824__auto____19887 = G__19885__19886.cljs$lang$protocol_mask$partition0$ & 131072;
                  if(or__3824__auto____19887) {
                    return or__3824__auto____19887
                  }else {
                    return G__19885__19886.cljs$core$IMeta$
                  }
                }()) {
                  return true
                }else {
                  if(!G__19885__19886.cljs$lang$protocol_mask$partition0$) {
                    return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__19885__19886)
                  }else {
                    return false
                  }
                }
              }else {
                return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__19885__19886)
              }
            }();
            if(cljs.core.truth_(and__3822__auto____19888)) {
              return cljs.core.meta.call(null, obj)
            }else {
              return and__3822__auto____19888
            }
          }else {
            return and__3822__auto____19884
          }
        }()) ? cljs.core.concat.call(null, cljs.core.PersistentVector.fromArray(["^"], true), pr_seq.call(null, cljs.core.meta.call(null, obj), opts), cljs.core.PersistentVector.fromArray([" "], true)) : null, function() {
          var and__3822__auto____19889 = !(obj == null);
          if(and__3822__auto____19889) {
            return obj.cljs$lang$type
          }else {
            return and__3822__auto____19889
          }
        }() ? obj.cljs$lang$ctorPrSeq(obj) : function() {
          var G__19890__19891 = obj;
          if(G__19890__19891) {
            if(function() {
              var or__3824__auto____19892 = G__19890__19891.cljs$lang$protocol_mask$partition0$ & 536870912;
              if(or__3824__auto____19892) {
                return or__3824__auto____19892
              }else {
                return G__19890__19891.cljs$core$IPrintable$
              }
            }()) {
              return true
            }else {
              if(!G__19890__19891.cljs$lang$protocol_mask$partition0$) {
                return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__19890__19891)
              }else {
                return false
              }
            }
          }else {
            return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__19890__19891)
          }
        }() ? cljs.core._pr_seq.call(null, obj, opts) : cljs.core.truth_(cljs.core.regexp_QMARK_.call(null, obj)) ? cljs.core.list.call(null, '#"', obj.source, '"') : "\ufdd0'else" ? cljs.core.list.call(null, "#<", [cljs.core.str(obj)].join(""), ">") : null)
      }else {
        return null
      }
    }
  }
};
cljs.core.pr_writer = function pr_writer(obj, writer, opts) {
  if(obj == null) {
    return cljs.core._write.call(null, writer, "nil")
  }else {
    if(void 0 === obj) {
      return cljs.core._write.call(null, writer, "#<undefined>")
    }else {
      if("\ufdd0'else") {
        if(cljs.core.truth_(function() {
          var and__3822__auto____19905 = cljs.core._lookup.call(null, opts, "\ufdd0'meta", null);
          if(cljs.core.truth_(and__3822__auto____19905)) {
            var and__3822__auto____19909 = function() {
              var G__19906__19907 = obj;
              if(G__19906__19907) {
                if(function() {
                  var or__3824__auto____19908 = G__19906__19907.cljs$lang$protocol_mask$partition0$ & 131072;
                  if(or__3824__auto____19908) {
                    return or__3824__auto____19908
                  }else {
                    return G__19906__19907.cljs$core$IMeta$
                  }
                }()) {
                  return true
                }else {
                  if(!G__19906__19907.cljs$lang$protocol_mask$partition0$) {
                    return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__19906__19907)
                  }else {
                    return false
                  }
                }
              }else {
                return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__19906__19907)
              }
            }();
            if(cljs.core.truth_(and__3822__auto____19909)) {
              return cljs.core.meta.call(null, obj)
            }else {
              return and__3822__auto____19909
            }
          }else {
            return and__3822__auto____19905
          }
        }())) {
          cljs.core._write.call(null, writer, "^");
          pr_writer.call(null, cljs.core.meta.call(null, obj), writer, opts);
          cljs.core._write.call(null, writer, " ")
        }else {
        }
        if(function() {
          var and__3822__auto____19910 = !(obj == null);
          if(and__3822__auto____19910) {
            return obj.cljs$lang$type
          }else {
            return and__3822__auto____19910
          }
        }()) {
          return obj.cljs$lang$ctorPrWriter(writer, opts)
        }else {
          if(function() {
            var G__19911__19912 = obj;
            if(G__19911__19912) {
              if(function() {
                var or__3824__auto____19913 = G__19911__19912.cljs$lang$protocol_mask$partition0$ & 2147483648;
                if(or__3824__auto____19913) {
                  return or__3824__auto____19913
                }else {
                  return G__19911__19912.cljs$core$IPrintWithWriter$
                }
              }()) {
                return true
              }else {
                if(!G__19911__19912.cljs$lang$protocol_mask$partition0$) {
                  return cljs.core.type_satisfies_.call(null, cljs.core.IPrintWithWriter, G__19911__19912)
                }else {
                  return false
                }
              }
            }else {
              return cljs.core.type_satisfies_.call(null, cljs.core.IPrintWithWriter, G__19911__19912)
            }
          }()) {
            return cljs.core._pr_writer.call(null, obj, writer, opts)
          }else {
            if(function() {
              var G__19914__19915 = obj;
              if(G__19914__19915) {
                if(function() {
                  var or__3824__auto____19916 = G__19914__19915.cljs$lang$protocol_mask$partition0$ & 536870912;
                  if(or__3824__auto____19916) {
                    return or__3824__auto____19916
                  }else {
                    return G__19914__19915.cljs$core$IPrintable$
                  }
                }()) {
                  return true
                }else {
                  if(!G__19914__19915.cljs$lang$protocol_mask$partition0$) {
                    return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__19914__19915)
                  }else {
                    return false
                  }
                }
              }else {
                return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__19914__19915)
              }
            }()) {
              return cljs.core.apply.call(null, cljs.core.write_all, writer, cljs.core._pr_seq.call(null, obj, opts))
            }else {
              if(cljs.core.truth_(cljs.core.regexp_QMARK_.call(null, obj))) {
                return cljs.core.write_all.call(null, writer, '#"', obj.source, '"')
              }else {
                if("\ufdd0'else") {
                  return cljs.core.write_all.call(null, writer, "#<", [cljs.core.str(obj)].join(""), ">")
                }else {
                  return null
                }
              }
            }
          }
        }
      }else {
        return null
      }
    }
  }
};
cljs.core.pr_seq_writer = function pr_seq_writer(objs, writer, opts) {
  cljs.core.pr_writer.call(null, cljs.core.first.call(null, objs), writer, opts);
  var G__19920__19921 = cljs.core.seq.call(null, cljs.core.next.call(null, objs));
  while(true) {
    if(G__19920__19921) {
      var obj__19922 = cljs.core.first.call(null, G__19920__19921);
      cljs.core._write.call(null, writer, " ");
      cljs.core.pr_writer.call(null, obj__19922, writer, opts);
      var G__19923 = cljs.core.next.call(null, G__19920__19921);
      G__19920__19921 = G__19923;
      continue
    }else {
      return null
    }
    break
  }
};
cljs.core.pr_sb_with_opts = function pr_sb_with_opts(objs, opts) {
  var sb__19926 = new goog.string.StringBuffer;
  var writer__19927 = new cljs.core.StringBufferWriter(sb__19926);
  cljs.core.pr_seq_writer.call(null, objs, writer__19927, opts);
  cljs.core._flush.call(null, writer__19927);
  return sb__19926
};
cljs.core.pr_str_with_opts = function pr_str_with_opts(objs, opts) {
  if(cljs.core.empty_QMARK_.call(null, objs)) {
    return""
  }else {
    return[cljs.core.str(cljs.core.pr_sb_with_opts.call(null, objs, opts))].join("")
  }
};
cljs.core.prn_str_with_opts = function prn_str_with_opts(objs, opts) {
  if(cljs.core.empty_QMARK_.call(null, objs)) {
    return"\n"
  }else {
    var sb__19929 = cljs.core.pr_sb_with_opts.call(null, objs, opts);
    sb__19929.append("\n");
    return[cljs.core.str(sb__19929)].join("")
  }
};
cljs.core.pr_with_opts = function pr_with_opts(objs, opts) {
  return cljs.core.string_print.call(null, cljs.core.pr_str_with_opts.call(null, objs, opts))
};
cljs.core.newline = function newline(opts) {
  cljs.core.string_print.call(null, "\n");
  if(cljs.core.truth_(cljs.core._lookup.call(null, opts, "\ufdd0'flush-on-newline", null))) {
    return cljs.core.flush.call(null)
  }else {
    return null
  }
};
cljs.core._STAR_flush_on_newline_STAR_ = true;
cljs.core._STAR_print_readably_STAR_ = true;
cljs.core._STAR_print_meta_STAR_ = false;
cljs.core._STAR_print_dup_STAR_ = false;
cljs.core.pr_opts = function pr_opts() {
  return cljs.core.ObjMap.fromObject(["\ufdd0'flush-on-newline", "\ufdd0'readably", "\ufdd0'meta", "\ufdd0'dup"], {"\ufdd0'flush-on-newline":cljs.core._STAR_flush_on_newline_STAR_, "\ufdd0'readably":cljs.core._STAR_print_readably_STAR_, "\ufdd0'meta":cljs.core._STAR_print_meta_STAR_, "\ufdd0'dup":cljs.core._STAR_print_dup_STAR_})
};
cljs.core.pr_str = function() {
  var pr_str__delegate = function(objs) {
    return cljs.core.pr_str_with_opts.call(null, objs, cljs.core.pr_opts.call(null))
  };
  var pr_str = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return pr_str__delegate.call(this, objs)
  };
  pr_str.cljs$lang$maxFixedArity = 0;
  pr_str.cljs$lang$applyTo = function(arglist__19930) {
    var objs = cljs.core.seq(arglist__19930);
    return pr_str__delegate(objs)
  };
  pr_str.cljs$lang$arity$variadic = pr_str__delegate;
  return pr_str
}();
cljs.core.prn_str = function() {
  var prn_str__delegate = function(objs) {
    return cljs.core.prn_str_with_opts.call(null, objs, cljs.core.pr_opts.call(null))
  };
  var prn_str = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return prn_str__delegate.call(this, objs)
  };
  prn_str.cljs$lang$maxFixedArity = 0;
  prn_str.cljs$lang$applyTo = function(arglist__19931) {
    var objs = cljs.core.seq(arglist__19931);
    return prn_str__delegate(objs)
  };
  prn_str.cljs$lang$arity$variadic = prn_str__delegate;
  return prn_str
}();
cljs.core.pr = function() {
  var pr__delegate = function(objs) {
    return cljs.core.pr_with_opts.call(null, objs, cljs.core.pr_opts.call(null))
  };
  var pr = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return pr__delegate.call(this, objs)
  };
  pr.cljs$lang$maxFixedArity = 0;
  pr.cljs$lang$applyTo = function(arglist__19932) {
    var objs = cljs.core.seq(arglist__19932);
    return pr__delegate(objs)
  };
  pr.cljs$lang$arity$variadic = pr__delegate;
  return pr
}();
cljs.core.print = function() {
  var cljs_core_print__delegate = function(objs) {
    return cljs.core.pr_with_opts.call(null, objs, cljs.core.assoc.call(null, cljs.core.pr_opts.call(null), "\ufdd0'readably", false))
  };
  var cljs_core_print = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return cljs_core_print__delegate.call(this, objs)
  };
  cljs_core_print.cljs$lang$maxFixedArity = 0;
  cljs_core_print.cljs$lang$applyTo = function(arglist__19933) {
    var objs = cljs.core.seq(arglist__19933);
    return cljs_core_print__delegate(objs)
  };
  cljs_core_print.cljs$lang$arity$variadic = cljs_core_print__delegate;
  return cljs_core_print
}();
cljs.core.print_str = function() {
  var print_str__delegate = function(objs) {
    return cljs.core.pr_str_with_opts.call(null, objs, cljs.core.assoc.call(null, cljs.core.pr_opts.call(null), "\ufdd0'readably", false))
  };
  var print_str = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return print_str__delegate.call(this, objs)
  };
  print_str.cljs$lang$maxFixedArity = 0;
  print_str.cljs$lang$applyTo = function(arglist__19934) {
    var objs = cljs.core.seq(arglist__19934);
    return print_str__delegate(objs)
  };
  print_str.cljs$lang$arity$variadic = print_str__delegate;
  return print_str
}();
cljs.core.println = function() {
  var println__delegate = function(objs) {
    cljs.core.pr_with_opts.call(null, objs, cljs.core.assoc.call(null, cljs.core.pr_opts.call(null), "\ufdd0'readably", false));
    return cljs.core.newline.call(null, cljs.core.pr_opts.call(null))
  };
  var println = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return println__delegate.call(this, objs)
  };
  println.cljs$lang$maxFixedArity = 0;
  println.cljs$lang$applyTo = function(arglist__19935) {
    var objs = cljs.core.seq(arglist__19935);
    return println__delegate(objs)
  };
  println.cljs$lang$arity$variadic = println__delegate;
  return println
}();
cljs.core.println_str = function() {
  var println_str__delegate = function(objs) {
    return cljs.core.prn_str_with_opts.call(null, objs, cljs.core.assoc.call(null, cljs.core.pr_opts.call(null), "\ufdd0'readably", false))
  };
  var println_str = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return println_str__delegate.call(this, objs)
  };
  println_str.cljs$lang$maxFixedArity = 0;
  println_str.cljs$lang$applyTo = function(arglist__19936) {
    var objs = cljs.core.seq(arglist__19936);
    return println_str__delegate(objs)
  };
  println_str.cljs$lang$arity$variadic = println_str__delegate;
  return println_str
}();
cljs.core.prn = function() {
  var prn__delegate = function(objs) {
    cljs.core.pr_with_opts.call(null, objs, cljs.core.pr_opts.call(null));
    return cljs.core.newline.call(null, cljs.core.pr_opts.call(null))
  };
  var prn = function(var_args) {
    var objs = null;
    if(goog.isDef(var_args)) {
      objs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
    }
    return prn__delegate.call(this, objs)
  };
  prn.cljs$lang$maxFixedArity = 0;
  prn.cljs$lang$applyTo = function(arglist__19937) {
    var objs = cljs.core.seq(arglist__19937);
    return prn__delegate(objs)
  };
  prn.cljs$lang$arity$variadic = prn__delegate;
  return prn
}();
cljs.core.printf = function() {
  var printf__delegate = function(fmt, args) {
    return cljs.core.print.call(null, cljs.core.apply.call(null, cljs.core.format, fmt, args))
  };
  var printf = function(fmt, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return printf__delegate.call(this, fmt, args)
  };
  printf.cljs$lang$maxFixedArity = 1;
  printf.cljs$lang$applyTo = function(arglist__19938) {
    var fmt = cljs.core.first(arglist__19938);
    var args = cljs.core.rest(arglist__19938);
    return printf__delegate(fmt, args)
  };
  printf.cljs$lang$arity$variadic = printf__delegate;
  return printf
}();
cljs.core.HashMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.HashMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__19939 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__19939, "{", ", ", "}", opts, coll)
};
cljs.core.IPrintable["number"] = true;
cljs.core._pr_seq["number"] = function(n, opts) {
  return cljs.core.list.call(null, [cljs.core.str(n)].join(""))
};
cljs.core.IndexedSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.IndexedSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.Subvec.prototype.cljs$core$IPrintable$ = true;
cljs.core.Subvec.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "[", " ", "]", opts, coll)
};
cljs.core.ChunkedCons.prototype.cljs$core$IPrintable$ = true;
cljs.core.ChunkedCons.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentTreeMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__19940 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__19940, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__19941 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__19941, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentQueue.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentQueue.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "#queue [", " ", "]", opts, cljs.core.seq.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.LazySeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.RSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.RSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentTreeSet.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "#{", " ", "}", opts, coll)
};
cljs.core.IPrintable["boolean"] = true;
cljs.core._pr_seq["boolean"] = function(bool, opts) {
  return cljs.core.list.call(null, [cljs.core.str(bool)].join(""))
};
cljs.core.IPrintable["string"] = true;
cljs.core._pr_seq["string"] = function(obj, opts) {
  if(cljs.core.keyword_QMARK_.call(null, obj)) {
    return cljs.core.list.call(null, [cljs.core.str(":"), cljs.core.str(function() {
      var temp__3974__auto____19942 = cljs.core.namespace.call(null, obj);
      if(cljs.core.truth_(temp__3974__auto____19942)) {
        var nspc__19943 = temp__3974__auto____19942;
        return[cljs.core.str(nspc__19943), cljs.core.str("/")].join("")
      }else {
        return null
      }
    }()), cljs.core.str(cljs.core.name.call(null, obj))].join(""))
  }else {
    if(cljs.core.symbol_QMARK_.call(null, obj)) {
      return cljs.core.list.call(null, [cljs.core.str(function() {
        var temp__3974__auto____19944 = cljs.core.namespace.call(null, obj);
        if(cljs.core.truth_(temp__3974__auto____19944)) {
          var nspc__19945 = temp__3974__auto____19944;
          return[cljs.core.str(nspc__19945), cljs.core.str("/")].join("")
        }else {
          return null
        }
      }()), cljs.core.str(cljs.core.name.call(null, obj))].join(""))
    }else {
      if("\ufdd0'else") {
        return cljs.core.list.call(null, cljs.core.truth_((new cljs.core.Keyword("\ufdd0'readably")).call(null, opts)) ? goog.string.quote(obj) : obj)
      }else {
        return null
      }
    }
  }
};
cljs.core.NodeSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.NodeSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.RedNode.prototype.cljs$core$IPrintable$ = true;
cljs.core.RedNode.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "[", " ", "]", opts, coll)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.ChunkedSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentHashMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__19946 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__19946, "{", ", ", "}", opts, coll)
};
cljs.core.Vector.prototype.cljs$core$IPrintable$ = true;
cljs.core.Vector.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "[", " ", "]", opts, coll)
};
cljs.core.PersistentHashSet.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentHashSet.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "#{", " ", "}", opts, coll)
};
cljs.core.PersistentVector.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentVector.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "[", " ", "]", opts, coll)
};
cljs.core.List.prototype.cljs$core$IPrintable$ = true;
cljs.core.List.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.IPrintable["array"] = true;
cljs.core._pr_seq["array"] = function(a, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "#<Array [", ", ", "]>", opts, a)
};
cljs.core.IPrintable["function"] = true;
cljs.core._pr_seq["function"] = function(this$) {
  return cljs.core.list.call(null, "#<", [cljs.core.str(this$)].join(""), ">")
};
cljs.core.EmptyList.prototype.cljs$core$IPrintable$ = true;
cljs.core.EmptyList.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.list.call(null, "()")
};
cljs.core.BlackNode.prototype.cljs$core$IPrintable$ = true;
cljs.core.BlackNode.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "[", " ", "]", opts, coll)
};
Date.prototype.cljs$core$IPrintable$ = true;
Date.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(d, _) {
  var normalize__19948 = function(n, len) {
    var ns__19947 = [cljs.core.str(n)].join("");
    while(true) {
      if(cljs.core.count.call(null, ns__19947) < len) {
        var G__19950 = [cljs.core.str("0"), cljs.core.str(ns__19947)].join("");
        ns__19947 = G__19950;
        continue
      }else {
        return ns__19947
      }
      break
    }
  };
  return cljs.core.list.call(null, [cljs.core.str('#inst "'), cljs.core.str(d.getUTCFullYear()), cljs.core.str("-"), cljs.core.str(normalize__19948.call(null, d.getUTCMonth() + 1, 2)), cljs.core.str("-"), cljs.core.str(normalize__19948.call(null, d.getUTCDate(), 2)), cljs.core.str("T"), cljs.core.str(normalize__19948.call(null, d.getUTCHours(), 2)), cljs.core.str(":"), cljs.core.str(normalize__19948.call(null, d.getUTCMinutes(), 2)), cljs.core.str(":"), cljs.core.str(normalize__19948.call(null, d.getUTCSeconds(), 
  2)), cljs.core.str("."), cljs.core.str(normalize__19948.call(null, d.getUTCMilliseconds(), 3)), cljs.core.str("-"), cljs.core.str('00:00"')].join(""))
};
cljs.core.Cons.prototype.cljs$core$IPrintable$ = true;
cljs.core.Cons.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.Range.prototype.cljs$core$IPrintable$ = true;
cljs.core.Range.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.ArrayNodeSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.ObjMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.ObjMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__19949 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__19949, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.HashMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.HashMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__19951 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__19951, "{", ", ", "}", opts, coll)
};
cljs.core.IPrintWithWriter["number"] = true;
cljs.core._pr_writer["number"] = function(n, writer, opts) {
  1 / 0;
  return cljs.core._write.call(null, writer, [cljs.core.str(n)].join(""))
};
cljs.core.IndexedSeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.IndexedSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.Subvec.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.Subvec.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "[", " ", "]", opts, coll)
};
cljs.core.ChunkedCons.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.ChunkedCons.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentTreeMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__19952 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__19952, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__19953 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__19953, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentQueue.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentQueue.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "#queue [", " ", "]", opts, cljs.core.seq.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.LazySeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.RSeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.RSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentTreeSet.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "#{", " ", "}", opts, coll)
};
cljs.core.IPrintWithWriter["boolean"] = true;
cljs.core._pr_writer["boolean"] = function(bool, writer, opts) {
  return cljs.core._write.call(null, writer, [cljs.core.str(bool)].join(""))
};
cljs.core.IPrintWithWriter["string"] = true;
cljs.core._pr_writer["string"] = function(obj, writer, opts) {
  if(cljs.core.keyword_QMARK_.call(null, obj)) {
    cljs.core._write.call(null, writer, ":");
    var temp__3974__auto____19954 = cljs.core.namespace.call(null, obj);
    if(cljs.core.truth_(temp__3974__auto____19954)) {
      var nspc__19955 = temp__3974__auto____19954;
      cljs.core.write_all.call(null, writer, [cljs.core.str(nspc__19955)].join(""), "/")
    }else {
    }
    return cljs.core._write.call(null, writer, cljs.core.name.call(null, obj))
  }else {
    if(cljs.core.symbol_QMARK_.call(null, obj)) {
      var temp__3974__auto____19956 = cljs.core.namespace.call(null, obj);
      if(cljs.core.truth_(temp__3974__auto____19956)) {
        var nspc__19957 = temp__3974__auto____19956;
        cljs.core.write_all.call(null, writer, [cljs.core.str(nspc__19957)].join(""), "/")
      }else {
      }
      return cljs.core._write.call(null, writer, cljs.core.name.call(null, obj))
    }else {
      if("\ufdd0'else") {
        if(cljs.core.truth_((new cljs.core.Keyword("\ufdd0'readably")).call(null, opts))) {
          return cljs.core._write.call(null, writer, goog.string.quote(obj))
        }else {
          return cljs.core._write.call(null, writer, obj)
        }
      }else {
        return null
      }
    }
  }
};
cljs.core.NodeSeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.NodeSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.RedNode.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.RedNode.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "[", " ", "]", opts, coll)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.ChunkedSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentHashMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__19958 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__19958, "{", ", ", "}", opts, coll)
};
cljs.core.Vector.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.Vector.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "[", " ", "]", opts, coll)
};
cljs.core.PersistentHashSet.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentHashSet.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "#{", " ", "}", opts, coll)
};
cljs.core.PersistentVector.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentVector.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "[", " ", "]", opts, coll)
};
cljs.core.List.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.List.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.IPrintWithWriter["array"] = true;
cljs.core._pr_writer["array"] = function(a, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "#<Array [", ", ", "]>", opts, a)
};
cljs.core.IPrintWithWriter["function"] = true;
cljs.core._pr_writer["function"] = function(this$, writer, _) {
  return cljs.core.write_all.call(null, writer, "#<", [cljs.core.str(this$)].join(""), ">")
};
cljs.core.EmptyList.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.EmptyList.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core._write.call(null, writer, "()")
};
cljs.core.BlackNode.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.BlackNode.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "[", " ", "]", opts, coll)
};
Date.prototype.cljs$core$IPrintWithWriter$ = true;
Date.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(d, writer, _) {
  var normalize__19960 = function(n, len) {
    var ns__19959 = [cljs.core.str(n)].join("");
    while(true) {
      if(cljs.core.count.call(null, ns__19959) < len) {
        var G__19962 = [cljs.core.str("0"), cljs.core.str(ns__19959)].join("");
        ns__19959 = G__19962;
        continue
      }else {
        return ns__19959
      }
      break
    }
  };
  return cljs.core.write_all.call(null, writer, '#inst "', [cljs.core.str(d.getUTCFullYear())].join(""), "-", normalize__19960.call(null, d.getUTCMonth() + 1, 2), "-", normalize__19960.call(null, d.getUTCDate(), 2), "T", normalize__19960.call(null, d.getUTCHours(), 2), ":", normalize__19960.call(null, d.getUTCMinutes(), 2), ":", normalize__19960.call(null, d.getUTCSeconds(), 2), ".", normalize__19960.call(null, d.getUTCMilliseconds(), 3), "-", '00:00"')
};
cljs.core.Cons.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.Cons.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.Range.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.Range.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.ArrayNodeSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.ObjMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.ObjMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__19961 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__19961, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "(", " ", ")", opts, coll)
};
cljs.core.PersistentVector.prototype.cljs$core$IComparable$ = true;
cljs.core.PersistentVector.prototype.cljs$core$IComparable$_compare$arity$2 = function(x, y) {
  return cljs.core.compare_indexed.call(null, x, y)
};
goog.provide("cljs.core.Atom");
cljs.core.Atom = function(state, meta, validator, watches) {
  this.state = state;
  this.meta = meta;
  this.validator = validator;
  this.watches = watches;
  this.cljs$lang$protocol_mask$partition0$ = 2690809856;
  this.cljs$lang$protocol_mask$partition1$ = 2
};
cljs.core.Atom.cljs$lang$type = true;
cljs.core.Atom.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Atom")
};
cljs.core.Atom.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Atom")
};
cljs.core.Atom.prototype.cljs$core$IHash$_hash$arity$1 = function(this$) {
  var this__19963 = this;
  return goog.getUid(this$)
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_notify_watches$arity$3 = function(this$, oldval, newval) {
  var this__19964 = this;
  var G__19965__19966 = cljs.core.seq.call(null, this__19964.watches);
  while(true) {
    if(G__19965__19966) {
      var vec__19967__19968 = cljs.core.first.call(null, G__19965__19966);
      var key__19969 = cljs.core.nth.call(null, vec__19967__19968, 0, null);
      var f__19970 = cljs.core.nth.call(null, vec__19967__19968, 1, null);
      f__19970.call(null, key__19969, this$, oldval, newval);
      var G__19978 = cljs.core.next.call(null, G__19965__19966);
      G__19965__19966 = G__19978;
      continue
    }else {
      return null
    }
    break
  }
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_add_watch$arity$3 = function(this$, key, f) {
  var this__19971 = this;
  return this$.watches = cljs.core.assoc.call(null, this__19971.watches, key, f)
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_remove_watch$arity$2 = function(this$, key) {
  var this__19972 = this;
  return this$.watches = cljs.core.dissoc.call(null, this__19972.watches, key)
};
cljs.core.Atom.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, writer, opts) {
  var this__19973 = this;
  cljs.core._write.call(null, writer, "#<Atom: ");
  cljs.core._pr_writer.call(null, this__19973.state, writer, opts);
  return cljs.core._write.call(null, writer, ">")
};
cljs.core.Atom.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(a, opts) {
  var this__19974 = this;
  return cljs.core.concat.call(null, cljs.core.PersistentVector.fromArray(["#<Atom: "], true), cljs.core._pr_seq.call(null, this__19974.state, opts), ">")
};
cljs.core.Atom.prototype.cljs$core$IMeta$_meta$arity$1 = function(_) {
  var this__19975 = this;
  return this__19975.meta
};
cljs.core.Atom.prototype.cljs$core$IDeref$_deref$arity$1 = function(_) {
  var this__19976 = this;
  return this__19976.state
};
cljs.core.Atom.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(o, other) {
  var this__19977 = this;
  return o === other
};
cljs.core.Atom;
cljs.core.atom = function() {
  var atom = null;
  var atom__1 = function(x) {
    return new cljs.core.Atom(x, null, null, null)
  };
  var atom__2 = function() {
    var G__19990__delegate = function(x, p__19979) {
      var map__19985__19986 = p__19979;
      var map__19985__19987 = cljs.core.seq_QMARK_.call(null, map__19985__19986) ? cljs.core.apply.call(null, cljs.core.hash_map, map__19985__19986) : map__19985__19986;
      var validator__19988 = cljs.core._lookup.call(null, map__19985__19987, "\ufdd0'validator", null);
      var meta__19989 = cljs.core._lookup.call(null, map__19985__19987, "\ufdd0'meta", null);
      return new cljs.core.Atom(x, meta__19989, validator__19988, null)
    };
    var G__19990 = function(x, var_args) {
      var p__19979 = null;
      if(goog.isDef(var_args)) {
        p__19979 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__19990__delegate.call(this, x, p__19979)
    };
    G__19990.cljs$lang$maxFixedArity = 1;
    G__19990.cljs$lang$applyTo = function(arglist__19991) {
      var x = cljs.core.first(arglist__19991);
      var p__19979 = cljs.core.rest(arglist__19991);
      return G__19990__delegate(x, p__19979)
    };
    G__19990.cljs$lang$arity$variadic = G__19990__delegate;
    return G__19990
  }();
  atom = function(x, var_args) {
    var p__19979 = var_args;
    switch(arguments.length) {
      case 1:
        return atom__1.call(this, x);
      default:
        return atom__2.cljs$lang$arity$variadic(x, cljs.core.array_seq(arguments, 1))
    }
    throw"Invalid arity: " + arguments.length;
  };
  atom.cljs$lang$maxFixedArity = 1;
  atom.cljs$lang$applyTo = atom__2.cljs$lang$applyTo;
  atom.cljs$lang$arity$1 = atom__1;
  atom.cljs$lang$arity$variadic = atom__2.cljs$lang$arity$variadic;
  return atom
}();
cljs.core.reset_BANG_ = function reset_BANG_(a, new_value) {
  var temp__3974__auto____19995 = a.validator;
  if(cljs.core.truth_(temp__3974__auto____19995)) {
    var validate__19996 = temp__3974__auto____19995;
    if(cljs.core.truth_(validate__19996.call(null, new_value))) {
    }else {
      throw new Error([cljs.core.str("Assert failed: "), cljs.core.str("Validator rejected reference state"), cljs.core.str("\n"), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'validate", "\ufdd1'new-value"), cljs.core.hash_map("\ufdd0'line", 6683))))].join(""));
    }
  }else {
  }
  var old_value__19997 = a.state;
  a.state = new_value;
  cljs.core._notify_watches.call(null, a, old_value__19997, new_value);
  return new_value
};
cljs.core.swap_BANG_ = function() {
  var swap_BANG_ = null;
  var swap_BANG___2 = function(a, f) {
    return cljs.core.reset_BANG_.call(null, a, f.call(null, a.state))
  };
  var swap_BANG___3 = function(a, f, x) {
    return cljs.core.reset_BANG_.call(null, a, f.call(null, a.state, x))
  };
  var swap_BANG___4 = function(a, f, x, y) {
    return cljs.core.reset_BANG_.call(null, a, f.call(null, a.state, x, y))
  };
  var swap_BANG___5 = function(a, f, x, y, z) {
    return cljs.core.reset_BANG_.call(null, a, f.call(null, a.state, x, y, z))
  };
  var swap_BANG___6 = function() {
    var G__19998__delegate = function(a, f, x, y, z, more) {
      return cljs.core.reset_BANG_.call(null, a, cljs.core.apply.call(null, f, a.state, x, y, z, more))
    };
    var G__19998 = function(a, f, x, y, z, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 5), 0)
      }
      return G__19998__delegate.call(this, a, f, x, y, z, more)
    };
    G__19998.cljs$lang$maxFixedArity = 5;
    G__19998.cljs$lang$applyTo = function(arglist__19999) {
      var a = cljs.core.first(arglist__19999);
      var f = cljs.core.first(cljs.core.next(arglist__19999));
      var x = cljs.core.first(cljs.core.next(cljs.core.next(arglist__19999)));
      var y = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__19999))));
      var z = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__19999)))));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__19999)))));
      return G__19998__delegate(a, f, x, y, z, more)
    };
    G__19998.cljs$lang$arity$variadic = G__19998__delegate;
    return G__19998
  }();
  swap_BANG_ = function(a, f, x, y, z, var_args) {
    var more = var_args;
    switch(arguments.length) {
      case 2:
        return swap_BANG___2.call(this, a, f);
      case 3:
        return swap_BANG___3.call(this, a, f, x);
      case 4:
        return swap_BANG___4.call(this, a, f, x, y);
      case 5:
        return swap_BANG___5.call(this, a, f, x, y, z);
      default:
        return swap_BANG___6.cljs$lang$arity$variadic(a, f, x, y, z, cljs.core.array_seq(arguments, 5))
    }
    throw"Invalid arity: " + arguments.length;
  };
  swap_BANG_.cljs$lang$maxFixedArity = 5;
  swap_BANG_.cljs$lang$applyTo = swap_BANG___6.cljs$lang$applyTo;
  swap_BANG_.cljs$lang$arity$2 = swap_BANG___2;
  swap_BANG_.cljs$lang$arity$3 = swap_BANG___3;
  swap_BANG_.cljs$lang$arity$4 = swap_BANG___4;
  swap_BANG_.cljs$lang$arity$5 = swap_BANG___5;
  swap_BANG_.cljs$lang$arity$variadic = swap_BANG___6.cljs$lang$arity$variadic;
  return swap_BANG_
}();
cljs.core.compare_and_set_BANG_ = function compare_and_set_BANG_(a, oldval, newval) {
  if(cljs.core._EQ_.call(null, a.state, oldval)) {
    cljs.core.reset_BANG_.call(null, a, newval);
    return true
  }else {
    return false
  }
};
cljs.core.deref = function deref(o) {
  return cljs.core._deref.call(null, o)
};
cljs.core.set_validator_BANG_ = function set_validator_BANG_(iref, val) {
  return iref.validator = val
};
cljs.core.get_validator = function get_validator(iref) {
  return iref.validator
};
cljs.core.alter_meta_BANG_ = function() {
  var alter_meta_BANG___delegate = function(iref, f, args) {
    return iref.meta = cljs.core.apply.call(null, f, iref.meta, args)
  };
  var alter_meta_BANG_ = function(iref, f, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return alter_meta_BANG___delegate.call(this, iref, f, args)
  };
  alter_meta_BANG_.cljs$lang$maxFixedArity = 2;
  alter_meta_BANG_.cljs$lang$applyTo = function(arglist__20000) {
    var iref = cljs.core.first(arglist__20000);
    var f = cljs.core.first(cljs.core.next(arglist__20000));
    var args = cljs.core.rest(cljs.core.next(arglist__20000));
    return alter_meta_BANG___delegate(iref, f, args)
  };
  alter_meta_BANG_.cljs$lang$arity$variadic = alter_meta_BANG___delegate;
  return alter_meta_BANG_
}();
cljs.core.reset_meta_BANG_ = function reset_meta_BANG_(iref, m) {
  return iref.meta = m
};
cljs.core.add_watch = function add_watch(iref, key, f) {
  return cljs.core._add_watch.call(null, iref, key, f)
};
cljs.core.remove_watch = function remove_watch(iref, key) {
  return cljs.core._remove_watch.call(null, iref, key)
};
cljs.core.gensym_counter = null;
cljs.core.gensym = function() {
  var gensym = null;
  var gensym__0 = function() {
    return gensym.call(null, "G__")
  };
  var gensym__1 = function(prefix_string) {
    if(cljs.core.gensym_counter == null) {
      cljs.core.gensym_counter = cljs.core.atom.call(null, 0)
    }else {
    }
    return cljs.core.symbol.call(null, [cljs.core.str(prefix_string), cljs.core.str(cljs.core.swap_BANG_.call(null, cljs.core.gensym_counter, cljs.core.inc))].join(""))
  };
  gensym = function(prefix_string) {
    switch(arguments.length) {
      case 0:
        return gensym__0.call(this);
      case 1:
        return gensym__1.call(this, prefix_string)
    }
    throw"Invalid arity: " + arguments.length;
  };
  gensym.cljs$lang$arity$0 = gensym__0;
  gensym.cljs$lang$arity$1 = gensym__1;
  return gensym
}();
cljs.core.fixture1 = 1;
cljs.core.fixture2 = 2;
goog.provide("cljs.core.Delay");
cljs.core.Delay = function(state, f) {
  this.state = state;
  this.f = f;
  this.cljs$lang$protocol_mask$partition1$ = 1;
  this.cljs$lang$protocol_mask$partition0$ = 32768
};
cljs.core.Delay.cljs$lang$type = true;
cljs.core.Delay.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/Delay")
};
cljs.core.Delay.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/Delay")
};
cljs.core.Delay.prototype.cljs$core$IPending$_realized_QMARK_$arity$1 = function(d) {
  var this__20001 = this;
  return(new cljs.core.Keyword("\ufdd0'done")).call(null, cljs.core.deref.call(null, this__20001.state))
};
cljs.core.Delay.prototype.cljs$core$IDeref$_deref$arity$1 = function(_) {
  var this__20002 = this;
  return(new cljs.core.Keyword("\ufdd0'value")).call(null, cljs.core.swap_BANG_.call(null, this__20002.state, function(p__20003) {
    var map__20004__20005 = p__20003;
    var map__20004__20006 = cljs.core.seq_QMARK_.call(null, map__20004__20005) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20004__20005) : map__20004__20005;
    var curr_state__20007 = map__20004__20006;
    var done__20008 = cljs.core._lookup.call(null, map__20004__20006, "\ufdd0'done", null);
    if(cljs.core.truth_(done__20008)) {
      return curr_state__20007
    }else {
      return cljs.core.ObjMap.fromObject(["\ufdd0'done", "\ufdd0'value"], {"\ufdd0'done":true, "\ufdd0'value":this__20002.f.call(null)})
    }
  }))
};
cljs.core.Delay;
cljs.core.delay_QMARK_ = function delay_QMARK_(x) {
  return cljs.core.instance_QMARK_.call(null, cljs.core.Delay, x)
};
cljs.core.force = function force(x) {
  if(cljs.core.delay_QMARK_.call(null, x)) {
    return cljs.core.deref.call(null, x)
  }else {
    return x
  }
};
cljs.core.realized_QMARK_ = function realized_QMARK_(d) {
  return cljs.core._realized_QMARK_.call(null, d)
};
cljs.core.js__GT_clj = function() {
  var js__GT_clj__delegate = function(x, options) {
    var map__20029__20030 = options;
    var map__20029__20031 = cljs.core.seq_QMARK_.call(null, map__20029__20030) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20029__20030) : map__20029__20030;
    var keywordize_keys__20032 = cljs.core._lookup.call(null, map__20029__20031, "\ufdd0'keywordize-keys", null);
    var keyfn__20033 = cljs.core.truth_(keywordize_keys__20032) ? cljs.core.keyword : cljs.core.str;
    var f__20048 = function thisfn(x) {
      if(cljs.core.seq_QMARK_.call(null, x)) {
        return cljs.core.doall.call(null, cljs.core.map.call(null, thisfn, x))
      }else {
        if(cljs.core.coll_QMARK_.call(null, x)) {
          return cljs.core.into.call(null, cljs.core.empty.call(null, x), cljs.core.map.call(null, thisfn, x))
        }else {
          if(cljs.core.truth_(goog.isArray(x))) {
            return cljs.core.vec.call(null, cljs.core.map.call(null, thisfn, x))
          }else {
            if(cljs.core.type.call(null, x) === Object) {
              return cljs.core.into.call(null, cljs.core.ObjMap.EMPTY, function() {
                var iter__2528__auto____20047 = function iter__20041(s__20042) {
                  return new cljs.core.LazySeq(null, false, function() {
                    var s__20042__20045 = s__20042;
                    while(true) {
                      if(cljs.core.seq.call(null, s__20042__20045)) {
                        var k__20046 = cljs.core.first.call(null, s__20042__20045);
                        return cljs.core.cons.call(null, cljs.core.PersistentVector.fromArray([keyfn__20033.call(null, k__20046), thisfn.call(null, x[k__20046])], true), iter__20041.call(null, cljs.core.rest.call(null, s__20042__20045)))
                      }else {
                        return null
                      }
                      break
                    }
                  }, null)
                };
                return iter__2528__auto____20047.call(null, cljs.core.js_keys.call(null, x))
              }())
            }else {
              if("\ufdd0'else") {
                return x
              }else {
                return null
              }
            }
          }
        }
      }
    };
    return f__20048.call(null, x)
  };
  var js__GT_clj = function(x, var_args) {
    var options = null;
    if(goog.isDef(var_args)) {
      options = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return js__GT_clj__delegate.call(this, x, options)
  };
  js__GT_clj.cljs$lang$maxFixedArity = 1;
  js__GT_clj.cljs$lang$applyTo = function(arglist__20049) {
    var x = cljs.core.first(arglist__20049);
    var options = cljs.core.rest(arglist__20049);
    return js__GT_clj__delegate(x, options)
  };
  js__GT_clj.cljs$lang$arity$variadic = js__GT_clj__delegate;
  return js__GT_clj
}();
cljs.core.memoize = function memoize(f) {
  var mem__20054 = cljs.core.atom.call(null, cljs.core.ObjMap.EMPTY);
  return function() {
    var G__20058__delegate = function(args) {
      var temp__3971__auto____20055 = cljs.core._lookup.call(null, cljs.core.deref.call(null, mem__20054), args, null);
      if(cljs.core.truth_(temp__3971__auto____20055)) {
        var v__20056 = temp__3971__auto____20055;
        return v__20056
      }else {
        var ret__20057 = cljs.core.apply.call(null, f, args);
        cljs.core.swap_BANG_.call(null, mem__20054, cljs.core.assoc, args, ret__20057);
        return ret__20057
      }
    };
    var G__20058 = function(var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__20058__delegate.call(this, args)
    };
    G__20058.cljs$lang$maxFixedArity = 0;
    G__20058.cljs$lang$applyTo = function(arglist__20059) {
      var args = cljs.core.seq(arglist__20059);
      return G__20058__delegate(args)
    };
    G__20058.cljs$lang$arity$variadic = G__20058__delegate;
    return G__20058
  }()
};
cljs.core.trampoline = function() {
  var trampoline = null;
  var trampoline__1 = function(f) {
    while(true) {
      var ret__20061 = f.call(null);
      if(cljs.core.fn_QMARK_.call(null, ret__20061)) {
        var G__20062 = ret__20061;
        f = G__20062;
        continue
      }else {
        return ret__20061
      }
      break
    }
  };
  var trampoline__2 = function() {
    var G__20063__delegate = function(f, args) {
      return trampoline.call(null, function() {
        return cljs.core.apply.call(null, f, args)
      })
    };
    var G__20063 = function(f, var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__20063__delegate.call(this, f, args)
    };
    G__20063.cljs$lang$maxFixedArity = 1;
    G__20063.cljs$lang$applyTo = function(arglist__20064) {
      var f = cljs.core.first(arglist__20064);
      var args = cljs.core.rest(arglist__20064);
      return G__20063__delegate(f, args)
    };
    G__20063.cljs$lang$arity$variadic = G__20063__delegate;
    return G__20063
  }();
  trampoline = function(f, var_args) {
    var args = var_args;
    switch(arguments.length) {
      case 1:
        return trampoline__1.call(this, f);
      default:
        return trampoline__2.cljs$lang$arity$variadic(f, cljs.core.array_seq(arguments, 1))
    }
    throw"Invalid arity: " + arguments.length;
  };
  trampoline.cljs$lang$maxFixedArity = 1;
  trampoline.cljs$lang$applyTo = trampoline__2.cljs$lang$applyTo;
  trampoline.cljs$lang$arity$1 = trampoline__1;
  trampoline.cljs$lang$arity$variadic = trampoline__2.cljs$lang$arity$variadic;
  return trampoline
}();
cljs.core.rand = function() {
  var rand = null;
  var rand__0 = function() {
    return rand.call(null, 1)
  };
  var rand__1 = function(n) {
    return Math.random.call(null) * n
  };
  rand = function(n) {
    switch(arguments.length) {
      case 0:
        return rand__0.call(this);
      case 1:
        return rand__1.call(this, n)
    }
    throw"Invalid arity: " + arguments.length;
  };
  rand.cljs$lang$arity$0 = rand__0;
  rand.cljs$lang$arity$1 = rand__1;
  return rand
}();
cljs.core.rand_int = function rand_int(n) {
  return Math.floor.call(null, Math.random.call(null) * n)
};
cljs.core.rand_nth = function rand_nth(coll) {
  return cljs.core.nth.call(null, coll, cljs.core.rand_int.call(null, cljs.core.count.call(null, coll)))
};
cljs.core.group_by = function group_by(f, coll) {
  return cljs.core.reduce.call(null, function(ret, x) {
    var k__20066 = f.call(null, x);
    return cljs.core.assoc.call(null, ret, k__20066, cljs.core.conj.call(null, cljs.core._lookup.call(null, ret, k__20066, cljs.core.PersistentVector.EMPTY), x))
  }, cljs.core.ObjMap.EMPTY, coll)
};
cljs.core.make_hierarchy = function make_hierarchy() {
  return cljs.core.ObjMap.fromObject(["\ufdd0'parents", "\ufdd0'descendants", "\ufdd0'ancestors"], {"\ufdd0'parents":cljs.core.ObjMap.EMPTY, "\ufdd0'descendants":cljs.core.ObjMap.EMPTY, "\ufdd0'ancestors":cljs.core.ObjMap.EMPTY})
};
cljs.core.global_hierarchy = cljs.core.atom.call(null, cljs.core.make_hierarchy.call(null));
cljs.core.isa_QMARK_ = function() {
  var isa_QMARK_ = null;
  var isa_QMARK___2 = function(child, parent) {
    return isa_QMARK_.call(null, cljs.core.deref.call(null, cljs.core.global_hierarchy), child, parent)
  };
  var isa_QMARK___3 = function(h, child, parent) {
    var or__3824__auto____20075 = cljs.core._EQ_.call(null, child, parent);
    if(or__3824__auto____20075) {
      return or__3824__auto____20075
    }else {
      var or__3824__auto____20076 = cljs.core.contains_QMARK_.call(null, (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h).call(null, child), parent);
      if(or__3824__auto____20076) {
        return or__3824__auto____20076
      }else {
        var and__3822__auto____20077 = cljs.core.vector_QMARK_.call(null, parent);
        if(and__3822__auto____20077) {
          var and__3822__auto____20078 = cljs.core.vector_QMARK_.call(null, child);
          if(and__3822__auto____20078) {
            var and__3822__auto____20079 = cljs.core.count.call(null, parent) === cljs.core.count.call(null, child);
            if(and__3822__auto____20079) {
              var ret__20080 = true;
              var i__20081 = 0;
              while(true) {
                if(function() {
                  var or__3824__auto____20082 = cljs.core.not.call(null, ret__20080);
                  if(or__3824__auto____20082) {
                    return or__3824__auto____20082
                  }else {
                    return i__20081 === cljs.core.count.call(null, parent)
                  }
                }()) {
                  return ret__20080
                }else {
                  var G__20083 = isa_QMARK_.call(null, h, child.call(null, i__20081), parent.call(null, i__20081));
                  var G__20084 = i__20081 + 1;
                  ret__20080 = G__20083;
                  i__20081 = G__20084;
                  continue
                }
                break
              }
            }else {
              return and__3822__auto____20079
            }
          }else {
            return and__3822__auto____20078
          }
        }else {
          return and__3822__auto____20077
        }
      }
    }
  };
  isa_QMARK_ = function(h, child, parent) {
    switch(arguments.length) {
      case 2:
        return isa_QMARK___2.call(this, h, child);
      case 3:
        return isa_QMARK___3.call(this, h, child, parent)
    }
    throw"Invalid arity: " + arguments.length;
  };
  isa_QMARK_.cljs$lang$arity$2 = isa_QMARK___2;
  isa_QMARK_.cljs$lang$arity$3 = isa_QMARK___3;
  return isa_QMARK_
}();
cljs.core.parents = function() {
  var parents = null;
  var parents__1 = function(tag) {
    return parents.call(null, cljs.core.deref.call(null, cljs.core.global_hierarchy), tag)
  };
  var parents__2 = function(h, tag) {
    return cljs.core.not_empty.call(null, cljs.core._lookup.call(null, (new cljs.core.Keyword("\ufdd0'parents")).call(null, h), tag, null))
  };
  parents = function(h, tag) {
    switch(arguments.length) {
      case 1:
        return parents__1.call(this, h);
      case 2:
        return parents__2.call(this, h, tag)
    }
    throw"Invalid arity: " + arguments.length;
  };
  parents.cljs$lang$arity$1 = parents__1;
  parents.cljs$lang$arity$2 = parents__2;
  return parents
}();
cljs.core.ancestors = function() {
  var ancestors = null;
  var ancestors__1 = function(tag) {
    return ancestors.call(null, cljs.core.deref.call(null, cljs.core.global_hierarchy), tag)
  };
  var ancestors__2 = function(h, tag) {
    return cljs.core.not_empty.call(null, cljs.core._lookup.call(null, (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h), tag, null))
  };
  ancestors = function(h, tag) {
    switch(arguments.length) {
      case 1:
        return ancestors__1.call(this, h);
      case 2:
        return ancestors__2.call(this, h, tag)
    }
    throw"Invalid arity: " + arguments.length;
  };
  ancestors.cljs$lang$arity$1 = ancestors__1;
  ancestors.cljs$lang$arity$2 = ancestors__2;
  return ancestors
}();
cljs.core.descendants = function() {
  var descendants = null;
  var descendants__1 = function(tag) {
    return descendants.call(null, cljs.core.deref.call(null, cljs.core.global_hierarchy), tag)
  };
  var descendants__2 = function(h, tag) {
    return cljs.core.not_empty.call(null, cljs.core._lookup.call(null, (new cljs.core.Keyword("\ufdd0'descendants")).call(null, h), tag, null))
  };
  descendants = function(h, tag) {
    switch(arguments.length) {
      case 1:
        return descendants__1.call(this, h);
      case 2:
        return descendants__2.call(this, h, tag)
    }
    throw"Invalid arity: " + arguments.length;
  };
  descendants.cljs$lang$arity$1 = descendants__1;
  descendants.cljs$lang$arity$2 = descendants__2;
  return descendants
}();
cljs.core.derive = function() {
  var derive = null;
  var derive__2 = function(tag, parent) {
    if(cljs.core.truth_(cljs.core.namespace.call(null, parent))) {
    }else {
      throw new Error([cljs.core.str("Assert failed: "), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'namespace", "\ufdd1'parent"), cljs.core.hash_map("\ufdd0'line", 6967))))].join(""));
    }
    cljs.core.swap_BANG_.call(null, cljs.core.global_hierarchy, derive, tag, parent);
    return null
  };
  var derive__3 = function(h, tag, parent) {
    if(cljs.core.not_EQ_.call(null, tag, parent)) {
    }else {
      throw new Error([cljs.core.str("Assert failed: "), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'not=", "\ufdd1'tag", "\ufdd1'parent"), cljs.core.hash_map("\ufdd0'line", 6971))))].join(""));
    }
    var tp__20093 = (new cljs.core.Keyword("\ufdd0'parents")).call(null, h);
    var td__20094 = (new cljs.core.Keyword("\ufdd0'descendants")).call(null, h);
    var ta__20095 = (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h);
    var tf__20096 = function(m, source, sources, target, targets) {
      return cljs.core.reduce.call(null, function(ret, k) {
        return cljs.core.assoc.call(null, ret, k, cljs.core.reduce.call(null, cljs.core.conj, cljs.core._lookup.call(null, targets, k, cljs.core.PersistentHashSet.EMPTY), cljs.core.cons.call(null, target, targets.call(null, target))))
      }, m, cljs.core.cons.call(null, source, sources.call(null, source)))
    };
    var or__3824__auto____20097 = cljs.core.contains_QMARK_.call(null, tp__20093.call(null, tag), parent) ? null : function() {
      if(cljs.core.contains_QMARK_.call(null, ta__20095.call(null, tag), parent)) {
        throw new Error([cljs.core.str(tag), cljs.core.str("already has"), cljs.core.str(parent), cljs.core.str("as ancestor")].join(""));
      }else {
      }
      if(cljs.core.contains_QMARK_.call(null, ta__20095.call(null, parent), tag)) {
        throw new Error([cljs.core.str("Cyclic derivation:"), cljs.core.str(parent), cljs.core.str("has"), cljs.core.str(tag), cljs.core.str("as ancestor")].join(""));
      }else {
      }
      return cljs.core.ObjMap.fromObject(["\ufdd0'parents", "\ufdd0'ancestors", "\ufdd0'descendants"], {"\ufdd0'parents":cljs.core.assoc.call(null, (new cljs.core.Keyword("\ufdd0'parents")).call(null, h), tag, cljs.core.conj.call(null, cljs.core._lookup.call(null, tp__20093, tag, cljs.core.PersistentHashSet.EMPTY), parent)), "\ufdd0'ancestors":tf__20096.call(null, (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h), tag, td__20094, parent, ta__20095), "\ufdd0'descendants":tf__20096.call(null, 
      (new cljs.core.Keyword("\ufdd0'descendants")).call(null, h), parent, ta__20095, tag, td__20094)})
    }();
    if(cljs.core.truth_(or__3824__auto____20097)) {
      return or__3824__auto____20097
    }else {
      return h
    }
  };
  derive = function(h, tag, parent) {
    switch(arguments.length) {
      case 2:
        return derive__2.call(this, h, tag);
      case 3:
        return derive__3.call(this, h, tag, parent)
    }
    throw"Invalid arity: " + arguments.length;
  };
  derive.cljs$lang$arity$2 = derive__2;
  derive.cljs$lang$arity$3 = derive__3;
  return derive
}();
cljs.core.underive = function() {
  var underive = null;
  var underive__2 = function(tag, parent) {
    cljs.core.swap_BANG_.call(null, cljs.core.global_hierarchy, underive, tag, parent);
    return null
  };
  var underive__3 = function(h, tag, parent) {
    var parentMap__20102 = (new cljs.core.Keyword("\ufdd0'parents")).call(null, h);
    var childsParents__20103 = cljs.core.truth_(parentMap__20102.call(null, tag)) ? cljs.core.disj.call(null, parentMap__20102.call(null, tag), parent) : cljs.core.PersistentHashSet.EMPTY;
    var newParents__20104 = cljs.core.truth_(cljs.core.not_empty.call(null, childsParents__20103)) ? cljs.core.assoc.call(null, parentMap__20102, tag, childsParents__20103) : cljs.core.dissoc.call(null, parentMap__20102, tag);
    var deriv_seq__20105 = cljs.core.flatten.call(null, cljs.core.map.call(null, function(p1__20085_SHARP_) {
      return cljs.core.cons.call(null, cljs.core.first.call(null, p1__20085_SHARP_), cljs.core.interpose.call(null, cljs.core.first.call(null, p1__20085_SHARP_), cljs.core.second.call(null, p1__20085_SHARP_)))
    }, cljs.core.seq.call(null, newParents__20104)));
    if(cljs.core.contains_QMARK_.call(null, parentMap__20102.call(null, tag), parent)) {
      return cljs.core.reduce.call(null, function(p1__20086_SHARP_, p2__20087_SHARP_) {
        return cljs.core.apply.call(null, cljs.core.derive, p1__20086_SHARP_, p2__20087_SHARP_)
      }, cljs.core.make_hierarchy.call(null), cljs.core.partition.call(null, 2, deriv_seq__20105))
    }else {
      return h
    }
  };
  underive = function(h, tag, parent) {
    switch(arguments.length) {
      case 2:
        return underive__2.call(this, h, tag);
      case 3:
        return underive__3.call(this, h, tag, parent)
    }
    throw"Invalid arity: " + arguments.length;
  };
  underive.cljs$lang$arity$2 = underive__2;
  underive.cljs$lang$arity$3 = underive__3;
  return underive
}();
cljs.core.reset_cache = function reset_cache(method_cache, method_table, cached_hierarchy, hierarchy) {
  cljs.core.swap_BANG_.call(null, method_cache, function(_) {
    return cljs.core.deref.call(null, method_table)
  });
  return cljs.core.swap_BANG_.call(null, cached_hierarchy, function(_) {
    return cljs.core.deref.call(null, hierarchy)
  })
};
cljs.core.prefers_STAR_ = function prefers_STAR_(x, y, prefer_table) {
  var xprefs__20113 = cljs.core.deref.call(null, prefer_table).call(null, x);
  var or__3824__auto____20115 = cljs.core.truth_(function() {
    var and__3822__auto____20114 = xprefs__20113;
    if(cljs.core.truth_(and__3822__auto____20114)) {
      return xprefs__20113.call(null, y)
    }else {
      return and__3822__auto____20114
    }
  }()) ? true : null;
  if(cljs.core.truth_(or__3824__auto____20115)) {
    return or__3824__auto____20115
  }else {
    var or__3824__auto____20117 = function() {
      var ps__20116 = cljs.core.parents.call(null, y);
      while(true) {
        if(cljs.core.count.call(null, ps__20116) > 0) {
          if(cljs.core.truth_(prefers_STAR_.call(null, x, cljs.core.first.call(null, ps__20116), prefer_table))) {
          }else {
          }
          var G__20120 = cljs.core.rest.call(null, ps__20116);
          ps__20116 = G__20120;
          continue
        }else {
          return null
        }
        break
      }
    }();
    if(cljs.core.truth_(or__3824__auto____20117)) {
      return or__3824__auto____20117
    }else {
      var or__3824__auto____20119 = function() {
        var ps__20118 = cljs.core.parents.call(null, x);
        while(true) {
          if(cljs.core.count.call(null, ps__20118) > 0) {
            if(cljs.core.truth_(prefers_STAR_.call(null, cljs.core.first.call(null, ps__20118), y, prefer_table))) {
            }else {
            }
            var G__20121 = cljs.core.rest.call(null, ps__20118);
            ps__20118 = G__20121;
            continue
          }else {
            return null
          }
          break
        }
      }();
      if(cljs.core.truth_(or__3824__auto____20119)) {
        return or__3824__auto____20119
      }else {
        return false
      }
    }
  }
};
cljs.core.dominates = function dominates(x, y, prefer_table) {
  var or__3824__auto____20123 = cljs.core.prefers_STAR_.call(null, x, y, prefer_table);
  if(cljs.core.truth_(or__3824__auto____20123)) {
    return or__3824__auto____20123
  }else {
    return cljs.core.isa_QMARK_.call(null, x, y)
  }
};
cljs.core.find_and_cache_best_method = function find_and_cache_best_method(name, dispatch_val, hierarchy, method_table, prefer_table, method_cache, cached_hierarchy) {
  var best_entry__20141 = cljs.core.reduce.call(null, function(be, p__20133) {
    var vec__20134__20135 = p__20133;
    var k__20136 = cljs.core.nth.call(null, vec__20134__20135, 0, null);
    var ___20137 = cljs.core.nth.call(null, vec__20134__20135, 1, null);
    var e__20138 = vec__20134__20135;
    if(cljs.core.isa_QMARK_.call(null, dispatch_val, k__20136)) {
      var be2__20140 = cljs.core.truth_(function() {
        var or__3824__auto____20139 = be == null;
        if(or__3824__auto____20139) {
          return or__3824__auto____20139
        }else {
          return cljs.core.dominates.call(null, k__20136, cljs.core.first.call(null, be), prefer_table)
        }
      }()) ? e__20138 : be;
      if(cljs.core.truth_(cljs.core.dominates.call(null, cljs.core.first.call(null, be2__20140), k__20136, prefer_table))) {
      }else {
        throw new Error([cljs.core.str("Multiple methods in multimethod '"), cljs.core.str(name), cljs.core.str("' match dispatch value: "), cljs.core.str(dispatch_val), cljs.core.str(" -> "), cljs.core.str(k__20136), cljs.core.str(" and "), cljs.core.str(cljs.core.first.call(null, be2__20140)), cljs.core.str(", and neither is preferred")].join(""));
      }
      return be2__20140
    }else {
      return be
    }
  }, null, cljs.core.deref.call(null, method_table));
  if(cljs.core.truth_(best_entry__20141)) {
    if(cljs.core._EQ_.call(null, cljs.core.deref.call(null, cached_hierarchy), cljs.core.deref.call(null, hierarchy))) {
      cljs.core.swap_BANG_.call(null, method_cache, cljs.core.assoc, dispatch_val, cljs.core.second.call(null, best_entry__20141));
      return cljs.core.second.call(null, best_entry__20141)
    }else {
      cljs.core.reset_cache.call(null, method_cache, method_table, cached_hierarchy, hierarchy);
      return find_and_cache_best_method.call(null, name, dispatch_val, hierarchy, method_table, prefer_table, method_cache, cached_hierarchy)
    }
  }else {
    return null
  }
};
cljs.core.IMultiFn = {};
cljs.core._reset = function _reset(mf) {
  if(function() {
    var and__3822__auto____20146 = mf;
    if(and__3822__auto____20146) {
      return mf.cljs$core$IMultiFn$_reset$arity$1
    }else {
      return and__3822__auto____20146
    }
  }()) {
    return mf.cljs$core$IMultiFn$_reset$arity$1(mf)
  }else {
    var x__2431__auto____20147 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20148 = cljs.core._reset[goog.typeOf(x__2431__auto____20147)];
      if(or__3824__auto____20148) {
        return or__3824__auto____20148
      }else {
        var or__3824__auto____20149 = cljs.core._reset["_"];
        if(or__3824__auto____20149) {
          return or__3824__auto____20149
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-reset", mf);
        }
      }
    }().call(null, mf)
  }
};
cljs.core._add_method = function _add_method(mf, dispatch_val, method) {
  if(function() {
    var and__3822__auto____20154 = mf;
    if(and__3822__auto____20154) {
      return mf.cljs$core$IMultiFn$_add_method$arity$3
    }else {
      return and__3822__auto____20154
    }
  }()) {
    return mf.cljs$core$IMultiFn$_add_method$arity$3(mf, dispatch_val, method)
  }else {
    var x__2431__auto____20155 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20156 = cljs.core._add_method[goog.typeOf(x__2431__auto____20155)];
      if(or__3824__auto____20156) {
        return or__3824__auto____20156
      }else {
        var or__3824__auto____20157 = cljs.core._add_method["_"];
        if(or__3824__auto____20157) {
          return or__3824__auto____20157
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-add-method", mf);
        }
      }
    }().call(null, mf, dispatch_val, method)
  }
};
cljs.core._remove_method = function _remove_method(mf, dispatch_val) {
  if(function() {
    var and__3822__auto____20162 = mf;
    if(and__3822__auto____20162) {
      return mf.cljs$core$IMultiFn$_remove_method$arity$2
    }else {
      return and__3822__auto____20162
    }
  }()) {
    return mf.cljs$core$IMultiFn$_remove_method$arity$2(mf, dispatch_val)
  }else {
    var x__2431__auto____20163 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20164 = cljs.core._remove_method[goog.typeOf(x__2431__auto____20163)];
      if(or__3824__auto____20164) {
        return or__3824__auto____20164
      }else {
        var or__3824__auto____20165 = cljs.core._remove_method["_"];
        if(or__3824__auto____20165) {
          return or__3824__auto____20165
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-remove-method", mf);
        }
      }
    }().call(null, mf, dispatch_val)
  }
};
cljs.core._prefer_method = function _prefer_method(mf, dispatch_val, dispatch_val_y) {
  if(function() {
    var and__3822__auto____20170 = mf;
    if(and__3822__auto____20170) {
      return mf.cljs$core$IMultiFn$_prefer_method$arity$3
    }else {
      return and__3822__auto____20170
    }
  }()) {
    return mf.cljs$core$IMultiFn$_prefer_method$arity$3(mf, dispatch_val, dispatch_val_y)
  }else {
    var x__2431__auto____20171 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20172 = cljs.core._prefer_method[goog.typeOf(x__2431__auto____20171)];
      if(or__3824__auto____20172) {
        return or__3824__auto____20172
      }else {
        var or__3824__auto____20173 = cljs.core._prefer_method["_"];
        if(or__3824__auto____20173) {
          return or__3824__auto____20173
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-prefer-method", mf);
        }
      }
    }().call(null, mf, dispatch_val, dispatch_val_y)
  }
};
cljs.core._get_method = function _get_method(mf, dispatch_val) {
  if(function() {
    var and__3822__auto____20178 = mf;
    if(and__3822__auto____20178) {
      return mf.cljs$core$IMultiFn$_get_method$arity$2
    }else {
      return and__3822__auto____20178
    }
  }()) {
    return mf.cljs$core$IMultiFn$_get_method$arity$2(mf, dispatch_val)
  }else {
    var x__2431__auto____20179 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20180 = cljs.core._get_method[goog.typeOf(x__2431__auto____20179)];
      if(or__3824__auto____20180) {
        return or__3824__auto____20180
      }else {
        var or__3824__auto____20181 = cljs.core._get_method["_"];
        if(or__3824__auto____20181) {
          return or__3824__auto____20181
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-get-method", mf);
        }
      }
    }().call(null, mf, dispatch_val)
  }
};
cljs.core._methods = function _methods(mf) {
  if(function() {
    var and__3822__auto____20186 = mf;
    if(and__3822__auto____20186) {
      return mf.cljs$core$IMultiFn$_methods$arity$1
    }else {
      return and__3822__auto____20186
    }
  }()) {
    return mf.cljs$core$IMultiFn$_methods$arity$1(mf)
  }else {
    var x__2431__auto____20187 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20188 = cljs.core._methods[goog.typeOf(x__2431__auto____20187)];
      if(or__3824__auto____20188) {
        return or__3824__auto____20188
      }else {
        var or__3824__auto____20189 = cljs.core._methods["_"];
        if(or__3824__auto____20189) {
          return or__3824__auto____20189
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-methods", mf);
        }
      }
    }().call(null, mf)
  }
};
cljs.core._prefers = function _prefers(mf) {
  if(function() {
    var and__3822__auto____20194 = mf;
    if(and__3822__auto____20194) {
      return mf.cljs$core$IMultiFn$_prefers$arity$1
    }else {
      return and__3822__auto____20194
    }
  }()) {
    return mf.cljs$core$IMultiFn$_prefers$arity$1(mf)
  }else {
    var x__2431__auto____20195 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20196 = cljs.core._prefers[goog.typeOf(x__2431__auto____20195)];
      if(or__3824__auto____20196) {
        return or__3824__auto____20196
      }else {
        var or__3824__auto____20197 = cljs.core._prefers["_"];
        if(or__3824__auto____20197) {
          return or__3824__auto____20197
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-prefers", mf);
        }
      }
    }().call(null, mf)
  }
};
cljs.core._dispatch = function _dispatch(mf, args) {
  if(function() {
    var and__3822__auto____20202 = mf;
    if(and__3822__auto____20202) {
      return mf.cljs$core$IMultiFn$_dispatch$arity$2
    }else {
      return and__3822__auto____20202
    }
  }()) {
    return mf.cljs$core$IMultiFn$_dispatch$arity$2(mf, args)
  }else {
    var x__2431__auto____20203 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20204 = cljs.core._dispatch[goog.typeOf(x__2431__auto____20203)];
      if(or__3824__auto____20204) {
        return or__3824__auto____20204
      }else {
        var or__3824__auto____20205 = cljs.core._dispatch["_"];
        if(or__3824__auto____20205) {
          return or__3824__auto____20205
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-dispatch", mf);
        }
      }
    }().call(null, mf, args)
  }
};
cljs.core.do_dispatch = function do_dispatch(mf, dispatch_fn, args) {
  var dispatch_val__20208 = cljs.core.apply.call(null, dispatch_fn, args);
  var target_fn__20209 = cljs.core._get_method.call(null, mf, dispatch_val__20208);
  if(cljs.core.truth_(target_fn__20209)) {
  }else {
    throw new Error([cljs.core.str("No method in multimethod '"), cljs.core.str(cljs.core.name), cljs.core.str("' for dispatch value: "), cljs.core.str(dispatch_val__20208)].join(""));
  }
  return cljs.core.apply.call(null, target_fn__20209, args)
};
goog.provide("cljs.core.MultiFn");
cljs.core.MultiFn = function(name, dispatch_fn, default_dispatch_val, hierarchy, method_table, prefer_table, method_cache, cached_hierarchy) {
  this.name = name;
  this.dispatch_fn = dispatch_fn;
  this.default_dispatch_val = default_dispatch_val;
  this.hierarchy = hierarchy;
  this.method_table = method_table;
  this.prefer_table = prefer_table;
  this.method_cache = method_cache;
  this.cached_hierarchy = cached_hierarchy;
  this.cljs$lang$protocol_mask$partition0$ = 4194304;
  this.cljs$lang$protocol_mask$partition1$ = 256
};
cljs.core.MultiFn.cljs$lang$type = true;
cljs.core.MultiFn.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/MultiFn")
};
cljs.core.MultiFn.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/MultiFn")
};
cljs.core.MultiFn.prototype.cljs$core$IHash$_hash$arity$1 = function(this$) {
  var this__20210 = this;
  return goog.getUid(this$)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_reset$arity$1 = function(mf) {
  var this__20211 = this;
  cljs.core.swap_BANG_.call(null, this__20211.method_table, function(mf) {
    return cljs.core.ObjMap.EMPTY
  });
  cljs.core.swap_BANG_.call(null, this__20211.method_cache, function(mf) {
    return cljs.core.ObjMap.EMPTY
  });
  cljs.core.swap_BANG_.call(null, this__20211.prefer_table, function(mf) {
    return cljs.core.ObjMap.EMPTY
  });
  cljs.core.swap_BANG_.call(null, this__20211.cached_hierarchy, function(mf) {
    return null
  });
  return mf
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_add_method$arity$3 = function(mf, dispatch_val, method) {
  var this__20212 = this;
  cljs.core.swap_BANG_.call(null, this__20212.method_table, cljs.core.assoc, dispatch_val, method);
  cljs.core.reset_cache.call(null, this__20212.method_cache, this__20212.method_table, this__20212.cached_hierarchy, this__20212.hierarchy);
  return mf
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_remove_method$arity$2 = function(mf, dispatch_val) {
  var this__20213 = this;
  cljs.core.swap_BANG_.call(null, this__20213.method_table, cljs.core.dissoc, dispatch_val);
  cljs.core.reset_cache.call(null, this__20213.method_cache, this__20213.method_table, this__20213.cached_hierarchy, this__20213.hierarchy);
  return mf
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_get_method$arity$2 = function(mf, dispatch_val) {
  var this__20214 = this;
  if(cljs.core._EQ_.call(null, cljs.core.deref.call(null, this__20214.cached_hierarchy), cljs.core.deref.call(null, this__20214.hierarchy))) {
  }else {
    cljs.core.reset_cache.call(null, this__20214.method_cache, this__20214.method_table, this__20214.cached_hierarchy, this__20214.hierarchy)
  }
  var temp__3971__auto____20215 = cljs.core.deref.call(null, this__20214.method_cache).call(null, dispatch_val);
  if(cljs.core.truth_(temp__3971__auto____20215)) {
    var target_fn__20216 = temp__3971__auto____20215;
    return target_fn__20216
  }else {
    var temp__3971__auto____20217 = cljs.core.find_and_cache_best_method.call(null, this__20214.name, dispatch_val, this__20214.hierarchy, this__20214.method_table, this__20214.prefer_table, this__20214.method_cache, this__20214.cached_hierarchy);
    if(cljs.core.truth_(temp__3971__auto____20217)) {
      var target_fn__20218 = temp__3971__auto____20217;
      return target_fn__20218
    }else {
      return cljs.core.deref.call(null, this__20214.method_table).call(null, this__20214.default_dispatch_val)
    }
  }
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_prefer_method$arity$3 = function(mf, dispatch_val_x, dispatch_val_y) {
  var this__20219 = this;
  if(cljs.core.truth_(cljs.core.prefers_STAR_.call(null, dispatch_val_x, dispatch_val_y, this__20219.prefer_table))) {
    throw new Error([cljs.core.str("Preference conflict in multimethod '"), cljs.core.str(this__20219.name), cljs.core.str("': "), cljs.core.str(dispatch_val_y), cljs.core.str(" is already preferred to "), cljs.core.str(dispatch_val_x)].join(""));
  }else {
  }
  cljs.core.swap_BANG_.call(null, this__20219.prefer_table, function(old) {
    return cljs.core.assoc.call(null, old, dispatch_val_x, cljs.core.conj.call(null, cljs.core._lookup.call(null, old, dispatch_val_x, cljs.core.PersistentHashSet.EMPTY), dispatch_val_y))
  });
  return cljs.core.reset_cache.call(null, this__20219.method_cache, this__20219.method_table, this__20219.cached_hierarchy, this__20219.hierarchy)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_methods$arity$1 = function(mf) {
  var this__20220 = this;
  return cljs.core.deref.call(null, this__20220.method_table)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_prefers$arity$1 = function(mf) {
  var this__20221 = this;
  return cljs.core.deref.call(null, this__20221.prefer_table)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_dispatch$arity$2 = function(mf, args) {
  var this__20222 = this;
  return cljs.core.do_dispatch.call(null, mf, this__20222.dispatch_fn, args)
};
cljs.core.MultiFn;
cljs.core.MultiFn.prototype.call = function() {
  var G__20224__delegate = function(_, args) {
    var self__20223 = this;
    return cljs.core._dispatch.call(null, self__20223, args)
  };
  var G__20224 = function(_, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return G__20224__delegate.call(this, _, args)
  };
  G__20224.cljs$lang$maxFixedArity = 1;
  G__20224.cljs$lang$applyTo = function(arglist__20225) {
    var _ = cljs.core.first(arglist__20225);
    var args = cljs.core.rest(arglist__20225);
    return G__20224__delegate(_, args)
  };
  G__20224.cljs$lang$arity$variadic = G__20224__delegate;
  return G__20224
}();
cljs.core.MultiFn.prototype.apply = function(_, args) {
  var self__20226 = this;
  return cljs.core._dispatch.call(null, self__20226, args)
};
cljs.core.remove_all_methods = function remove_all_methods(multifn) {
  return cljs.core._reset.call(null, multifn)
};
cljs.core.remove_method = function remove_method(multifn, dispatch_val) {
  return cljs.core._remove_method.call(null, multifn, dispatch_val)
};
cljs.core.prefer_method = function prefer_method(multifn, dispatch_val_x, dispatch_val_y) {
  return cljs.core._prefer_method.call(null, multifn, dispatch_val_x, dispatch_val_y)
};
cljs.core.methods$ = function methods$(multifn) {
  return cljs.core._methods.call(null, multifn)
};
cljs.core.get_method = function get_method(multifn, dispatch_val) {
  return cljs.core._get_method.call(null, multifn, dispatch_val)
};
cljs.core.prefers = function prefers(multifn) {
  return cljs.core._prefers.call(null, multifn)
};
goog.provide("cljs.core.UUID");
cljs.core.UUID = function(uuid) {
  this.uuid = uuid;
  this.cljs$lang$protocol_mask$partition1$ = 0;
  this.cljs$lang$protocol_mask$partition0$ = 2690646016
};
cljs.core.UUID.cljs$lang$type = true;
cljs.core.UUID.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.core/UUID")
};
cljs.core.UUID.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.core/UUID")
};
cljs.core.UUID.prototype.cljs$core$IHash$_hash$arity$1 = function(this$) {
  var this__20227 = this;
  return goog.string.hashCode(cljs.core.pr_str.call(null, this$))
};
cljs.core.UUID.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(_20229, writer, _) {
  var this__20228 = this;
  return cljs.core._write.call(null, writer, [cljs.core.str('#uuid "'), cljs.core.str(this__20228.uuid), cljs.core.str('"')].join(""))
};
cljs.core.UUID.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(_20231, _) {
  var this__20230 = this;
  return cljs.core.list.call(null, [cljs.core.str('#uuid "'), cljs.core.str(this__20230.uuid), cljs.core.str('"')].join(""))
};
cljs.core.UUID.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(_, other) {
  var this__20232 = this;
  var and__3822__auto____20233 = cljs.core.instance_QMARK_.call(null, cljs.core.UUID, other);
  if(and__3822__auto____20233) {
    return this__20232.uuid === other.uuid
  }else {
    return and__3822__auto____20233
  }
};
cljs.core.UUID.prototype.toString = function() {
  var this__20234 = this;
  var this__20235 = this;
  return cljs.core.pr_str.call(null, this__20235)
};
cljs.core.UUID;
goog.provide("carneades.policy_analysis.web.test.navigation");
goog.require("cljs.core");
carneades.policy_analysis.web.test.navigation.run = function run() {
  return cljs.core._EQ_.call(null, 4, 4)
};
goog.provide("jayq.util");
goog.require("cljs.core");
jayq.util.map__GT_js = function map__GT_js(m) {
  var out__20243 = {};
  var G__20244__20245 = cljs.core.seq.call(null, m);
  while(true) {
    if(G__20244__20245) {
      var vec__20246__20247 = cljs.core.first.call(null, G__20244__20245);
      var k__20248 = cljs.core.nth.call(null, vec__20246__20247, 0, null);
      var v__20249 = cljs.core.nth.call(null, vec__20246__20247, 1, null);
      out__20243[cljs.core.name.call(null, k__20248)] = v__20249;
      var G__20250 = cljs.core.next.call(null, G__20244__20245);
      G__20244__20245 = G__20250;
      continue
    }else {
    }
    break
  }
  return out__20243
};
jayq.util.wait = function wait(ms, func) {
  return setTimeout(func, ms)
};
jayq.util.log = function() {
  var log__delegate = function(v, text) {
    var vs__20252 = cljs.core.string_QMARK_.call(null, v) ? cljs.core.apply.call(null, cljs.core.str, v, text) : v;
    return console.log(vs__20252)
  };
  var log = function(v, var_args) {
    var text = null;
    if(goog.isDef(var_args)) {
      text = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return log__delegate.call(this, v, text)
  };
  log.cljs$lang$maxFixedArity = 1;
  log.cljs$lang$applyTo = function(arglist__20253) {
    var v = cljs.core.first(arglist__20253);
    var text = cljs.core.rest(arglist__20253);
    return log__delegate(v, text)
  };
  log.cljs$lang$arity$variadic = log__delegate;
  return log
}();
jayq.util.clj__GT_js = function clj__GT_js(x) {
  if(cljs.core.string_QMARK_.call(null, x)) {
    return x
  }else {
    if(cljs.core.keyword_QMARK_.call(null, x)) {
      return cljs.core.name.call(null, x)
    }else {
      if(cljs.core.map_QMARK_.call(null, x)) {
        var obj__20261 = {};
        var G__20262__20263 = cljs.core.seq.call(null, x);
        while(true) {
          if(G__20262__20263) {
            var vec__20264__20265 = cljs.core.first.call(null, G__20262__20263);
            var k__20266 = cljs.core.nth.call(null, vec__20264__20265, 0, null);
            var v__20267 = cljs.core.nth.call(null, vec__20264__20265, 1, null);
            obj__20261[clj__GT_js.call(null, k__20266)] = clj__GT_js.call(null, v__20267);
            var G__20268 = cljs.core.next.call(null, G__20262__20263);
            G__20262__20263 = G__20268;
            continue
          }else {
          }
          break
        }
        return obj__20261
      }else {
        if(cljs.core.sequential_QMARK_.call(null, x)) {
          return cljs.core.apply.call(null, cljs.core.array, cljs.core.map.call(null, clj__GT_js, x))
        }else {
          if("\ufdd0'else") {
            return x
          }else {
            return null
          }
        }
      }
    }
  }
};
goog.provide("cljs.reader");
goog.require("cljs.core");
goog.require("goog.string");
cljs.reader.PushbackReader = {};
cljs.reader.read_char = function read_char(reader) {
  if(function() {
    var and__3822__auto____20455 = reader;
    if(and__3822__auto____20455) {
      return reader.cljs$reader$PushbackReader$read_char$arity$1
    }else {
      return and__3822__auto____20455
    }
  }()) {
    return reader.cljs$reader$PushbackReader$read_char$arity$1(reader)
  }else {
    var x__2431__auto____20456 = reader == null ? null : reader;
    return function() {
      var or__3824__auto____20457 = cljs.reader.read_char[goog.typeOf(x__2431__auto____20456)];
      if(or__3824__auto____20457) {
        return or__3824__auto____20457
      }else {
        var or__3824__auto____20458 = cljs.reader.read_char["_"];
        if(or__3824__auto____20458) {
          return or__3824__auto____20458
        }else {
          throw cljs.core.missing_protocol.call(null, "PushbackReader.read-char", reader);
        }
      }
    }().call(null, reader)
  }
};
cljs.reader.unread = function unread(reader, ch) {
  if(function() {
    var and__3822__auto____20463 = reader;
    if(and__3822__auto____20463) {
      return reader.cljs$reader$PushbackReader$unread$arity$2
    }else {
      return and__3822__auto____20463
    }
  }()) {
    return reader.cljs$reader$PushbackReader$unread$arity$2(reader, ch)
  }else {
    var x__2431__auto____20464 = reader == null ? null : reader;
    return function() {
      var or__3824__auto____20465 = cljs.reader.unread[goog.typeOf(x__2431__auto____20464)];
      if(or__3824__auto____20465) {
        return or__3824__auto____20465
      }else {
        var or__3824__auto____20466 = cljs.reader.unread["_"];
        if(or__3824__auto____20466) {
          return or__3824__auto____20466
        }else {
          throw cljs.core.missing_protocol.call(null, "PushbackReader.unread", reader);
        }
      }
    }().call(null, reader, ch)
  }
};
goog.provide("cljs.reader.StringPushbackReader");
cljs.reader.StringPushbackReader = function(s, index_atom, buffer_atom) {
  this.s = s;
  this.index_atom = index_atom;
  this.buffer_atom = buffer_atom
};
cljs.reader.StringPushbackReader.cljs$lang$type = true;
cljs.reader.StringPushbackReader.cljs$lang$ctorPrSeq = function(this__2366__auto__) {
  return cljs.core.list.call(null, "cljs.reader/StringPushbackReader")
};
cljs.reader.StringPushbackReader.cljs$lang$ctorPrWriter = function(this__2366__auto__, writer__2367__auto__) {
  return cljs.core._write.call(null, writer__2367__auto__, "cljs.reader/StringPushbackReader")
};
cljs.reader.StringPushbackReader.prototype.cljs$reader$PushbackReader$ = true;
cljs.reader.StringPushbackReader.prototype.cljs$reader$PushbackReader$read_char$arity$1 = function(reader) {
  var this__20467 = this;
  if(cljs.core.empty_QMARK_.call(null, cljs.core.deref.call(null, this__20467.buffer_atom))) {
    var idx__20468 = cljs.core.deref.call(null, this__20467.index_atom);
    cljs.core.swap_BANG_.call(null, this__20467.index_atom, cljs.core.inc);
    return this__20467.s[idx__20468]
  }else {
    var buf__20469 = cljs.core.deref.call(null, this__20467.buffer_atom);
    cljs.core.swap_BANG_.call(null, this__20467.buffer_atom, cljs.core.rest);
    return cljs.core.first.call(null, buf__20469)
  }
};
cljs.reader.StringPushbackReader.prototype.cljs$reader$PushbackReader$unread$arity$2 = function(reader, ch) {
  var this__20470 = this;
  return cljs.core.swap_BANG_.call(null, this__20470.buffer_atom, function(p1__20450_SHARP_) {
    return cljs.core.cons.call(null, ch, p1__20450_SHARP_)
  })
};
cljs.reader.StringPushbackReader;
cljs.reader.push_back_reader = function push_back_reader(s) {
  return new cljs.reader.StringPushbackReader(s, cljs.core.atom.call(null, 0), cljs.core.atom.call(null, null))
};
cljs.reader.whitespace_QMARK_ = function whitespace_QMARK_(ch) {
  var or__3824__auto____20472 = goog.string.isBreakingWhitespace(ch);
  if(cljs.core.truth_(or__3824__auto____20472)) {
    return or__3824__auto____20472
  }else {
    return"," === ch
  }
};
cljs.reader.numeric_QMARK_ = function numeric_QMARK_(ch) {
  return goog.string.isNumeric(ch)
};
cljs.reader.comment_prefix_QMARK_ = function comment_prefix_QMARK_(ch) {
  return";" === ch
};
cljs.reader.number_literal_QMARK_ = function number_literal_QMARK_(reader, initch) {
  var or__3824__auto____20477 = cljs.reader.numeric_QMARK_.call(null, initch);
  if(or__3824__auto____20477) {
    return or__3824__auto____20477
  }else {
    var and__3822__auto____20479 = function() {
      var or__3824__auto____20478 = "+" === initch;
      if(or__3824__auto____20478) {
        return or__3824__auto____20478
      }else {
        return"-" === initch
      }
    }();
    if(cljs.core.truth_(and__3822__auto____20479)) {
      return cljs.reader.numeric_QMARK_.call(null, function() {
        var next_ch__20480 = cljs.reader.read_char.call(null, reader);
        cljs.reader.unread.call(null, reader, next_ch__20480);
        return next_ch__20480
      }())
    }else {
      return and__3822__auto____20479
    }
  }
};
cljs.reader.reader_error = function() {
  var reader_error__delegate = function(rdr, msg) {
    throw new Error(cljs.core.apply.call(null, cljs.core.str, msg));
  };
  var reader_error = function(rdr, var_args) {
    var msg = null;
    if(goog.isDef(var_args)) {
      msg = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return reader_error__delegate.call(this, rdr, msg)
  };
  reader_error.cljs$lang$maxFixedArity = 1;
  reader_error.cljs$lang$applyTo = function(arglist__20481) {
    var rdr = cljs.core.first(arglist__20481);
    var msg = cljs.core.rest(arglist__20481);
    return reader_error__delegate(rdr, msg)
  };
  reader_error.cljs$lang$arity$variadic = reader_error__delegate;
  return reader_error
}();
cljs.reader.macro_terminating_QMARK_ = function macro_terminating_QMARK_(ch) {
  var and__3822__auto____20485 = !(ch === "#");
  if(and__3822__auto____20485) {
    var and__3822__auto____20486 = !(ch === "'");
    if(and__3822__auto____20486) {
      var and__3822__auto____20487 = !(ch === ":");
      if(and__3822__auto____20487) {
        return cljs.reader.macros.call(null, ch)
      }else {
        return and__3822__auto____20487
      }
    }else {
      return and__3822__auto____20486
    }
  }else {
    return and__3822__auto____20485
  }
};
cljs.reader.read_token = function read_token(rdr, initch) {
  var sb__20492 = new goog.string.StringBuffer(initch);
  var ch__20493 = cljs.reader.read_char.call(null, rdr);
  while(true) {
    if(function() {
      var or__3824__auto____20494 = ch__20493 == null;
      if(or__3824__auto____20494) {
        return or__3824__auto____20494
      }else {
        var or__3824__auto____20495 = cljs.reader.whitespace_QMARK_.call(null, ch__20493);
        if(or__3824__auto____20495) {
          return or__3824__auto____20495
        }else {
          return cljs.reader.macro_terminating_QMARK_.call(null, ch__20493)
        }
      }
    }()) {
      cljs.reader.unread.call(null, rdr, ch__20493);
      return sb__20492.toString()
    }else {
      var G__20496 = function() {
        sb__20492.append(ch__20493);
        return sb__20492
      }();
      var G__20497 = cljs.reader.read_char.call(null, rdr);
      sb__20492 = G__20496;
      ch__20493 = G__20497;
      continue
    }
    break
  }
};
cljs.reader.skip_line = function skip_line(reader, _) {
  while(true) {
    var ch__20501 = cljs.reader.read_char.call(null, reader);
    if(function() {
      var or__3824__auto____20502 = ch__20501 === "n";
      if(or__3824__auto____20502) {
        return or__3824__auto____20502
      }else {
        var or__3824__auto____20503 = ch__20501 === "r";
        if(or__3824__auto____20503) {
          return or__3824__auto____20503
        }else {
          return ch__20501 == null
        }
      }
    }()) {
      return reader
    }else {
      continue
    }
    break
  }
};
cljs.reader.int_pattern = cljs.core.re_pattern.call(null, "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
cljs.reader.ratio_pattern = cljs.core.re_pattern.call(null, "([-+]?[0-9]+)/([0-9]+)");
cljs.reader.float_pattern = cljs.core.re_pattern.call(null, "([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");
cljs.reader.symbol_pattern = cljs.core.re_pattern.call(null, "[:]?([^0-9/].*/)?([^0-9/][^/]*)");
cljs.reader.re_find_STAR_ = function re_find_STAR_(re, s) {
  var matches__20505 = re.exec(s);
  if(matches__20505 == null) {
    return null
  }else {
    if(matches__20505.length === 1) {
      return matches__20505[0]
    }else {
      return matches__20505
    }
  }
};
cljs.reader.match_int = function match_int(s) {
  var groups__20513 = cljs.reader.re_find_STAR_.call(null, cljs.reader.int_pattern, s);
  var group3__20514 = groups__20513[2];
  if(!function() {
    var or__3824__auto____20515 = group3__20514 == null;
    if(or__3824__auto____20515) {
      return or__3824__auto____20515
    }else {
      return group3__20514.length < 1
    }
  }()) {
    return 0
  }else {
    var negate__20516 = "-" === groups__20513[1] ? -1 : 1;
    var a__20517 = cljs.core.truth_(groups__20513[3]) ? [groups__20513[3], 10] : cljs.core.truth_(groups__20513[4]) ? [groups__20513[4], 16] : cljs.core.truth_(groups__20513[5]) ? [groups__20513[5], 8] : cljs.core.truth_(groups__20513[7]) ? [groups__20513[7], parseInt(groups__20513[7])] : "\ufdd0'default" ? [null, null] : null;
    var n__20518 = a__20517[0];
    var radix__20519 = a__20517[1];
    if(n__20518 == null) {
      return null
    }else {
      return negate__20516 * parseInt(n__20518, radix__20519)
    }
  }
};
cljs.reader.match_ratio = function match_ratio(s) {
  var groups__20523 = cljs.reader.re_find_STAR_.call(null, cljs.reader.ratio_pattern, s);
  var numinator__20524 = groups__20523[1];
  var denominator__20525 = groups__20523[2];
  return parseInt(numinator__20524) / parseInt(denominator__20525)
};
cljs.reader.match_float = function match_float(s) {
  return parseFloat(s)
};
cljs.reader.re_matches_STAR_ = function re_matches_STAR_(re, s) {
  var matches__20528 = re.exec(s);
  if(function() {
    var and__3822__auto____20529 = !(matches__20528 == null);
    if(and__3822__auto____20529) {
      return matches__20528[0] === s
    }else {
      return and__3822__auto____20529
    }
  }()) {
    if(matches__20528.length === 1) {
      return matches__20528[0]
    }else {
      return matches__20528
    }
  }else {
    return null
  }
};
cljs.reader.match_number = function match_number(s) {
  if(cljs.core.truth_(cljs.reader.re_matches_STAR_.call(null, cljs.reader.int_pattern, s))) {
    return cljs.reader.match_int.call(null, s)
  }else {
    if(cljs.core.truth_(cljs.reader.re_matches_STAR_.call(null, cljs.reader.ratio_pattern, s))) {
      return cljs.reader.match_ratio.call(null, s)
    }else {
      if(cljs.core.truth_(cljs.reader.re_matches_STAR_.call(null, cljs.reader.float_pattern, s))) {
        return cljs.reader.match_float.call(null, s)
      }else {
        return null
      }
    }
  }
};
cljs.reader.escape_char_map = function escape_char_map(c) {
  if(c === "t") {
    return"\t"
  }else {
    if(c === "r") {
      return"\r"
    }else {
      if(c === "n") {
        return"\n"
      }else {
        if(c === "\\") {
          return"\\"
        }else {
          if(c === '"') {
            return'"'
          }else {
            if(c === "b") {
              return"\u0008"
            }else {
              if(c === "f") {
                return"\u000c"
              }else {
                if("\ufdd0'else") {
                  return null
                }else {
                  return null
                }
              }
            }
          }
        }
      }
    }
  }
};
cljs.reader.read_2_chars = function read_2_chars(reader) {
  return(new goog.string.StringBuffer(cljs.reader.read_char.call(null, reader), cljs.reader.read_char.call(null, reader))).toString()
};
cljs.reader.read_4_chars = function read_4_chars(reader) {
  return(new goog.string.StringBuffer(cljs.reader.read_char.call(null, reader), cljs.reader.read_char.call(null, reader), cljs.reader.read_char.call(null, reader), cljs.reader.read_char.call(null, reader))).toString()
};
cljs.reader.unicode_2_pattern = cljs.core.re_pattern.call(null, "[0-9A-Fa-f]{2}");
cljs.reader.unicode_4_pattern = cljs.core.re_pattern.call(null, "[0-9A-Fa-f]{4}");
cljs.reader.validate_unicode_escape = function validate_unicode_escape(unicode_pattern, reader, escape_char, unicode_str) {
  if(cljs.core.truth_(cljs.core.re_matches.call(null, unicode_pattern, unicode_str))) {
    return unicode_str
  }else {
    return cljs.reader.reader_error.call(null, reader, "Unexpected unicode escape \\", escape_char, unicode_str)
  }
};
cljs.reader.make_unicode_char = function make_unicode_char(code_str) {
  var code__20531 = parseInt(code_str, 16);
  return String.fromCharCode(code__20531)
};
cljs.reader.escape_char = function escape_char(buffer, reader) {
  var ch__20534 = cljs.reader.read_char.call(null, reader);
  var mapresult__20535 = cljs.reader.escape_char_map.call(null, ch__20534);
  if(cljs.core.truth_(mapresult__20535)) {
    return mapresult__20535
  }else {
    if(ch__20534 === "x") {
      return cljs.reader.make_unicode_char.call(null, cljs.reader.validate_unicode_escape.call(null, cljs.reader.unicode_2_pattern, reader, ch__20534, cljs.reader.read_2_chars.call(null, reader)))
    }else {
      if(ch__20534 === "u") {
        return cljs.reader.make_unicode_char.call(null, cljs.reader.validate_unicode_escape.call(null, cljs.reader.unicode_4_pattern, reader, ch__20534, cljs.reader.read_4_chars.call(null, reader)))
      }else {
        if(cljs.reader.numeric_QMARK_.call(null, ch__20534)) {
          return String.fromCharCode(ch__20534)
        }else {
          if("\ufdd0'else") {
            return cljs.reader.reader_error.call(null, reader, "Unexpected unicode escape \\", ch__20534)
          }else {
            return null
          }
        }
      }
    }
  }
};
cljs.reader.read_past = function read_past(pred, rdr) {
  var ch__20537 = cljs.reader.read_char.call(null, rdr);
  while(true) {
    if(cljs.core.truth_(pred.call(null, ch__20537))) {
      var G__20538 = cljs.reader.read_char.call(null, rdr);
      ch__20537 = G__20538;
      continue
    }else {
      return ch__20537
    }
    break
  }
};
cljs.reader.read_delimited_list = function read_delimited_list(delim, rdr, recursive_QMARK_) {
  var a__20545 = cljs.core.transient$.call(null, cljs.core.PersistentVector.EMPTY);
  while(true) {
    var ch__20546 = cljs.reader.read_past.call(null, cljs.reader.whitespace_QMARK_, rdr);
    if(cljs.core.truth_(ch__20546)) {
    }else {
      cljs.reader.reader_error.call(null, rdr, "EOF while reading")
    }
    if(delim === ch__20546) {
      return cljs.core.persistent_BANG_.call(null, a__20545)
    }else {
      var temp__3971__auto____20547 = cljs.reader.macros.call(null, ch__20546);
      if(cljs.core.truth_(temp__3971__auto____20547)) {
        var macrofn__20548 = temp__3971__auto____20547;
        var mret__20549 = macrofn__20548.call(null, rdr, ch__20546);
        var G__20551 = mret__20549 === rdr ? a__20545 : cljs.core.conj_BANG_.call(null, a__20545, mret__20549);
        a__20545 = G__20551;
        continue
      }else {
        cljs.reader.unread.call(null, rdr, ch__20546);
        var o__20550 = cljs.reader.read.call(null, rdr, true, null, recursive_QMARK_);
        var G__20552 = o__20550 === rdr ? a__20545 : cljs.core.conj_BANG_.call(null, a__20545, o__20550);
        a__20545 = G__20552;
        continue
      }
    }
    break
  }
};
cljs.reader.not_implemented = function not_implemented(rdr, ch) {
  return cljs.reader.reader_error.call(null, rdr, "Reader for ", ch, " not implemented yet")
};
cljs.reader.read_dispatch = function read_dispatch(rdr, _) {
  var ch__20557 = cljs.reader.read_char.call(null, rdr);
  var dm__20558 = cljs.reader.dispatch_macros.call(null, ch__20557);
  if(cljs.core.truth_(dm__20558)) {
    return dm__20558.call(null, rdr, _)
  }else {
    var temp__3971__auto____20559 = cljs.reader.maybe_read_tagged_type.call(null, rdr, ch__20557);
    if(cljs.core.truth_(temp__3971__auto____20559)) {
      var obj__20560 = temp__3971__auto____20559;
      return obj__20560
    }else {
      return cljs.reader.reader_error.call(null, rdr, "No dispatch macro for ", ch__20557)
    }
  }
};
cljs.reader.read_unmatched_delimiter = function read_unmatched_delimiter(rdr, ch) {
  return cljs.reader.reader_error.call(null, rdr, "Unmached delimiter ", ch)
};
cljs.reader.read_list = function read_list(rdr, _) {
  return cljs.core.apply.call(null, cljs.core.list, cljs.reader.read_delimited_list.call(null, ")", rdr, true))
};
cljs.reader.read_comment = cljs.reader.skip_line;
cljs.reader.read_vector = function read_vector(rdr, _) {
  return cljs.reader.read_delimited_list.call(null, "]", rdr, true)
};
cljs.reader.read_map = function read_map(rdr, _) {
  var l__20562 = cljs.reader.read_delimited_list.call(null, "}", rdr, true);
  if(cljs.core.odd_QMARK_.call(null, cljs.core.count.call(null, l__20562))) {
    cljs.reader.reader_error.call(null, rdr, "Map literal must contain an even number of forms")
  }else {
  }
  return cljs.core.apply.call(null, cljs.core.hash_map, l__20562)
};
cljs.reader.read_number = function read_number(reader, initch) {
  var buffer__20569 = new goog.string.StringBuffer(initch);
  var ch__20570 = cljs.reader.read_char.call(null, reader);
  while(true) {
    if(cljs.core.truth_(function() {
      var or__3824__auto____20571 = ch__20570 == null;
      if(or__3824__auto____20571) {
        return or__3824__auto____20571
      }else {
        var or__3824__auto____20572 = cljs.reader.whitespace_QMARK_.call(null, ch__20570);
        if(or__3824__auto____20572) {
          return or__3824__auto____20572
        }else {
          return cljs.reader.macros.call(null, ch__20570)
        }
      }
    }())) {
      cljs.reader.unread.call(null, reader, ch__20570);
      var s__20573 = buffer__20569.toString();
      var or__3824__auto____20574 = cljs.reader.match_number.call(null, s__20573);
      if(cljs.core.truth_(or__3824__auto____20574)) {
        return or__3824__auto____20574
      }else {
        return cljs.reader.reader_error.call(null, reader, "Invalid number format [", s__20573, "]")
      }
    }else {
      var G__20575 = function() {
        buffer__20569.append(ch__20570);
        return buffer__20569
      }();
      var G__20576 = cljs.reader.read_char.call(null, reader);
      buffer__20569 = G__20575;
      ch__20570 = G__20576;
      continue
    }
    break
  }
};
cljs.reader.read_string_STAR_ = function read_string_STAR_(reader, _) {
  var buffer__20579 = new goog.string.StringBuffer;
  var ch__20580 = cljs.reader.read_char.call(null, reader);
  while(true) {
    if(ch__20580 == null) {
      return cljs.reader.reader_error.call(null, reader, "EOF while reading")
    }else {
      if("\\" === ch__20580) {
        var G__20581 = function() {
          buffer__20579.append(cljs.reader.escape_char.call(null, buffer__20579, reader));
          return buffer__20579
        }();
        var G__20582 = cljs.reader.read_char.call(null, reader);
        buffer__20579 = G__20581;
        ch__20580 = G__20582;
        continue
      }else {
        if('"' === ch__20580) {
          return buffer__20579.toString()
        }else {
          if("\ufdd0'default") {
            var G__20583 = function() {
              buffer__20579.append(ch__20580);
              return buffer__20579
            }();
            var G__20584 = cljs.reader.read_char.call(null, reader);
            buffer__20579 = G__20583;
            ch__20580 = G__20584;
            continue
          }else {
            return null
          }
        }
      }
    }
    break
  }
};
cljs.reader.special_symbols = function special_symbols(t, not_found) {
  if(t === "nil") {
    return null
  }else {
    if(t === "true") {
      return true
    }else {
      if(t === "false") {
        return false
      }else {
        if("\ufdd0'else") {
          return not_found
        }else {
          return null
        }
      }
    }
  }
};
cljs.reader.read_symbol = function read_symbol(reader, initch) {
  var token__20586 = cljs.reader.read_token.call(null, reader, initch);
  if(cljs.core.truth_(goog.string.contains(token__20586, "/"))) {
    return cljs.core.symbol.call(null, cljs.core.subs.call(null, token__20586, 0, token__20586.indexOf("/")), cljs.core.subs.call(null, token__20586, token__20586.indexOf("/") + 1, token__20586.length))
  }else {
    return cljs.reader.special_symbols.call(null, token__20586, cljs.core.symbol.call(null, token__20586))
  }
};
cljs.reader.read_keyword = function read_keyword(reader, initch) {
  var token__20596 = cljs.reader.read_token.call(null, reader, cljs.reader.read_char.call(null, reader));
  var a__20597 = cljs.reader.re_matches_STAR_.call(null, cljs.reader.symbol_pattern, token__20596);
  var token__20598 = a__20597[0];
  var ns__20599 = a__20597[1];
  var name__20600 = a__20597[2];
  if(cljs.core.truth_(function() {
    var or__3824__auto____20602 = function() {
      var and__3822__auto____20601 = !(void 0 === ns__20599);
      if(and__3822__auto____20601) {
        return ns__20599.substring(ns__20599.length - 2, ns__20599.length) === ":/"
      }else {
        return and__3822__auto____20601
      }
    }();
    if(cljs.core.truth_(or__3824__auto____20602)) {
      return or__3824__auto____20602
    }else {
      var or__3824__auto____20603 = name__20600[name__20600.length - 1] === ":";
      if(or__3824__auto____20603) {
        return or__3824__auto____20603
      }else {
        return!(token__20598.indexOf("::", 1) === -1)
      }
    }
  }())) {
    return cljs.reader.reader_error.call(null, reader, "Invalid token: ", token__20598)
  }else {
    if(function() {
      var and__3822__auto____20604 = !(ns__20599 == null);
      if(and__3822__auto____20604) {
        return ns__20599.length > 0
      }else {
        return and__3822__auto____20604
      }
    }()) {
      return cljs.core.keyword.call(null, ns__20599.substring(0, ns__20599.indexOf("/")), name__20600)
    }else {
      return cljs.core.keyword.call(null, token__20598)
    }
  }
};
cljs.reader.desugar_meta = function desugar_meta(f) {
  if(cljs.core.symbol_QMARK_.call(null, f)) {
    return cljs.core.ObjMap.fromObject(["\ufdd0'tag"], {"\ufdd0'tag":f})
  }else {
    if(cljs.core.string_QMARK_.call(null, f)) {
      return cljs.core.ObjMap.fromObject(["\ufdd0'tag"], {"\ufdd0'tag":f})
    }else {
      if(cljs.core.keyword_QMARK_.call(null, f)) {
        return cljs.core.PersistentArrayMap.fromArrays([f], [true])
      }else {
        if("\ufdd0'else") {
          return f
        }else {
          return null
        }
      }
    }
  }
};
cljs.reader.wrapping_reader = function wrapping_reader(sym) {
  return function(rdr, _) {
    return cljs.core.list.call(null, sym, cljs.reader.read.call(null, rdr, true, null, true))
  }
};
cljs.reader.throwing_reader = function throwing_reader(msg) {
  return function(rdr, _) {
    return cljs.reader.reader_error.call(null, rdr, msg)
  }
};
cljs.reader.read_meta = function read_meta(rdr, _) {
  var m__20610 = cljs.reader.desugar_meta.call(null, cljs.reader.read.call(null, rdr, true, null, true));
  if(cljs.core.map_QMARK_.call(null, m__20610)) {
  }else {
    cljs.reader.reader_error.call(null, rdr, "Metadata must be Symbol,Keyword,String or Map")
  }
  var o__20611 = cljs.reader.read.call(null, rdr, true, null, true);
  if(function() {
    var G__20612__20613 = o__20611;
    if(G__20612__20613) {
      if(function() {
        var or__3824__auto____20614 = G__20612__20613.cljs$lang$protocol_mask$partition0$ & 262144;
        if(or__3824__auto____20614) {
          return or__3824__auto____20614
        }else {
          return G__20612__20613.cljs$core$IWithMeta$
        }
      }()) {
        return true
      }else {
        if(!G__20612__20613.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IWithMeta, G__20612__20613)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IWithMeta, G__20612__20613)
    }
  }()) {
    return cljs.core.with_meta.call(null, o__20611, cljs.core.merge.call(null, cljs.core.meta.call(null, o__20611), m__20610))
  }else {
    return cljs.reader.reader_error.call(null, rdr, "Metadata can only be applied to IWithMetas")
  }
};
cljs.reader.read_set = function read_set(rdr, _) {
  return cljs.core.set.call(null, cljs.reader.read_delimited_list.call(null, "}", rdr, true))
};
cljs.reader.read_regex = function read_regex(rdr, ch) {
  return cljs.core.re_pattern.call(null, cljs.reader.read_string_STAR_.call(null, rdr, ch))
};
cljs.reader.read_discard = function read_discard(rdr, _) {
  cljs.reader.read.call(null, rdr, true, null, true);
  return rdr
};
cljs.reader.macros = function macros(c) {
  if(c === '"') {
    return cljs.reader.read_string_STAR_
  }else {
    if(c === ":") {
      return cljs.reader.read_keyword
    }else {
      if(c === ";") {
        return cljs.reader.not_implemented
      }else {
        if(c === "'") {
          return cljs.reader.wrapping_reader.call(null, "\ufdd1'quote")
        }else {
          if(c === "@") {
            return cljs.reader.wrapping_reader.call(null, "\ufdd1'deref")
          }else {
            if(c === "^") {
              return cljs.reader.read_meta
            }else {
              if(c === "`") {
                return cljs.reader.not_implemented
              }else {
                if(c === "~") {
                  return cljs.reader.not_implemented
                }else {
                  if(c === "(") {
                    return cljs.reader.read_list
                  }else {
                    if(c === ")") {
                      return cljs.reader.read_unmatched_delimiter
                    }else {
                      if(c === "[") {
                        return cljs.reader.read_vector
                      }else {
                        if(c === "]") {
                          return cljs.reader.read_unmatched_delimiter
                        }else {
                          if(c === "{") {
                            return cljs.reader.read_map
                          }else {
                            if(c === "}") {
                              return cljs.reader.read_unmatched_delimiter
                            }else {
                              if(c === "\\") {
                                return cljs.reader.read_char
                              }else {
                                if(c === "%") {
                                  return cljs.reader.not_implemented
                                }else {
                                  if(c === "#") {
                                    return cljs.reader.read_dispatch
                                  }else {
                                    if("\ufdd0'else") {
                                      return null
                                    }else {
                                      return null
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};
cljs.reader.dispatch_macros = function dispatch_macros(s) {
  if(s === "{") {
    return cljs.reader.read_set
  }else {
    if(s === "<") {
      return cljs.reader.throwing_reader.call(null, "Unreadable form")
    }else {
      if(s === '"') {
        return cljs.reader.read_regex
      }else {
        if(s === "!") {
          return cljs.reader.read_comment
        }else {
          if(s === "_") {
            return cljs.reader.read_discard
          }else {
            if("\ufdd0'else") {
              return null
            }else {
              return null
            }
          }
        }
      }
    }
  }
};
cljs.reader.read = function read(reader, eof_is_error, sentinel, is_recursive) {
  while(true) {
    var ch__20618 = cljs.reader.read_char.call(null, reader);
    if(ch__20618 == null) {
      if(cljs.core.truth_(eof_is_error)) {
        return cljs.reader.reader_error.call(null, reader, "EOF while reading")
      }else {
        return sentinel
      }
    }else {
      if(cljs.reader.whitespace_QMARK_.call(null, ch__20618)) {
        var G__20621 = reader;
        var G__20622 = eof_is_error;
        var G__20623 = sentinel;
        var G__20624 = is_recursive;
        reader = G__20621;
        eof_is_error = G__20622;
        sentinel = G__20623;
        is_recursive = G__20624;
        continue
      }else {
        if(cljs.reader.comment_prefix_QMARK_.call(null, ch__20618)) {
          var G__20625 = cljs.reader.read_comment.call(null, reader, ch__20618);
          var G__20626 = eof_is_error;
          var G__20627 = sentinel;
          var G__20628 = is_recursive;
          reader = G__20625;
          eof_is_error = G__20626;
          sentinel = G__20627;
          is_recursive = G__20628;
          continue
        }else {
          if("\ufdd0'else") {
            var f__20619 = cljs.reader.macros.call(null, ch__20618);
            var res__20620 = cljs.core.truth_(f__20619) ? f__20619.call(null, reader, ch__20618) : cljs.reader.number_literal_QMARK_.call(null, reader, ch__20618) ? cljs.reader.read_number.call(null, reader, ch__20618) : "\ufdd0'else" ? cljs.reader.read_symbol.call(null, reader, ch__20618) : null;
            if(res__20620 === reader) {
              var G__20629 = reader;
              var G__20630 = eof_is_error;
              var G__20631 = sentinel;
              var G__20632 = is_recursive;
              reader = G__20629;
              eof_is_error = G__20630;
              sentinel = G__20631;
              is_recursive = G__20632;
              continue
            }else {
              return res__20620
            }
          }else {
            return null
          }
        }
      }
    }
    break
  }
};
cljs.reader.read_string = function read_string(s) {
  var r__20634 = cljs.reader.push_back_reader.call(null, s);
  return cljs.reader.read.call(null, r__20634, true, null, false)
};
cljs.reader.zero_fill_right = function zero_fill_right(s, width) {
  if(cljs.core._EQ_.call(null, width, cljs.core.count.call(null, s))) {
    return s
  }else {
    if(width < cljs.core.count.call(null, s)) {
      return s.substring(0, width)
    }else {
      if("\ufdd0'else") {
        var b__20636 = new goog.string.StringBuffer(s);
        while(true) {
          if(b__20636.getLength() < width) {
            var G__20637 = b__20636.append("0");
            b__20636 = G__20637;
            continue
          }else {
            return b__20636.toString()
          }
          break
        }
      }else {
        return null
      }
    }
  }
};
cljs.reader.divisible_QMARK_ = function divisible_QMARK_(num, div) {
  return num % div === 0
};
cljs.reader.indivisible_QMARK_ = function indivisible_QMARK_(num, div) {
  return cljs.core.not.call(null, cljs.reader.divisible_QMARK_.call(null, num, div))
};
cljs.reader.leap_year_QMARK_ = function leap_year_QMARK_(year) {
  var and__3822__auto____20640 = cljs.reader.divisible_QMARK_.call(null, year, 4);
  if(cljs.core.truth_(and__3822__auto____20640)) {
    var or__3824__auto____20641 = cljs.reader.indivisible_QMARK_.call(null, year, 100);
    if(cljs.core.truth_(or__3824__auto____20641)) {
      return or__3824__auto____20641
    }else {
      return cljs.reader.divisible_QMARK_.call(null, year, 400)
    }
  }else {
    return and__3822__auto____20640
  }
};
cljs.reader.days_in_month = function() {
  var dim_norm__20646 = cljs.core.PersistentVector.fromArray([null, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], true);
  var dim_leap__20647 = cljs.core.PersistentVector.fromArray([null, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], true);
  return function(month, leap_year_QMARK_) {
    return cljs.core._lookup.call(null, cljs.core.truth_(leap_year_QMARK_) ? dim_leap__20647 : dim_norm__20646, month, null)
  }
}();
cljs.reader.parse_and_validate_timestamp = function() {
  var timestamp__20648 = /(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?)?)?)?(?:[Z]|([-+])(\d\d):(\d\d))?/;
  var check__20650 = function(low, n, high, msg) {
    if(function() {
      var and__3822__auto____20649 = low <= n;
      if(and__3822__auto____20649) {
        return n <= high
      }else {
        return and__3822__auto____20649
      }
    }()) {
    }else {
      throw new Error([cljs.core.str("Assert failed: "), cljs.core.str([cljs.core.str(msg), cljs.core.str(" Failed:  "), cljs.core.str(low), cljs.core.str("<="), cljs.core.str(n), cljs.core.str("<="), cljs.core.str(high)].join("")), cljs.core.str("\n"), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'<=", "\ufdd1'low", "\ufdd1'n", "\ufdd1'high"), cljs.core.hash_map("\ufdd0'line", 474))))].join(""));
    }
    return n
  };
  return function(ts) {
    var temp__3974__auto____20651 = cljs.core.map.call(null, cljs.core.vec, cljs.core.split_at.call(null, 8, cljs.core.re_matches.call(null, timestamp__20648, ts)));
    if(cljs.core.truth_(temp__3974__auto____20651)) {
      var vec__20652__20655 = temp__3974__auto____20651;
      var vec__20653__20656 = cljs.core.nth.call(null, vec__20652__20655, 0, null);
      var ___20657 = cljs.core.nth.call(null, vec__20653__20656, 0, null);
      var years__20658 = cljs.core.nth.call(null, vec__20653__20656, 1, null);
      var months__20659 = cljs.core.nth.call(null, vec__20653__20656, 2, null);
      var days__20660 = cljs.core.nth.call(null, vec__20653__20656, 3, null);
      var hours__20661 = cljs.core.nth.call(null, vec__20653__20656, 4, null);
      var minutes__20662 = cljs.core.nth.call(null, vec__20653__20656, 5, null);
      var seconds__20663 = cljs.core.nth.call(null, vec__20653__20656, 6, null);
      var milliseconds__20664 = cljs.core.nth.call(null, vec__20653__20656, 7, null);
      var vec__20654__20665 = cljs.core.nth.call(null, vec__20652__20655, 1, null);
      var ___20666 = cljs.core.nth.call(null, vec__20654__20665, 0, null);
      var ___20667 = cljs.core.nth.call(null, vec__20654__20665, 1, null);
      var ___20668 = cljs.core.nth.call(null, vec__20654__20665, 2, null);
      var V__20669 = vec__20652__20655;
      var vec__20670__20673 = cljs.core.map.call(null, function(v) {
        return cljs.core.map.call(null, function(p1__20645_SHARP_) {
          return parseInt(p1__20645_SHARP_, 10)
        }, v)
      }, cljs.core.map.call(null, function(p1__20643_SHARP_, p2__20642_SHARP_) {
        return cljs.core.update_in.call(null, p2__20642_SHARP_, cljs.core.PersistentVector.fromArray([0], true), p1__20643_SHARP_)
      }, cljs.core.PersistentVector.fromArray([cljs.core.constantly.call(null, null), function(p1__20644_SHARP_) {
        if(cljs.core._EQ_.call(null, p1__20644_SHARP_, "-")) {
          return"-1"
        }else {
          return"1"
        }
      }], true), V__20669));
      var vec__20671__20674 = cljs.core.nth.call(null, vec__20670__20673, 0, null);
      var ___20675 = cljs.core.nth.call(null, vec__20671__20674, 0, null);
      var y__20676 = cljs.core.nth.call(null, vec__20671__20674, 1, null);
      var mo__20677 = cljs.core.nth.call(null, vec__20671__20674, 2, null);
      var d__20678 = cljs.core.nth.call(null, vec__20671__20674, 3, null);
      var h__20679 = cljs.core.nth.call(null, vec__20671__20674, 4, null);
      var m__20680 = cljs.core.nth.call(null, vec__20671__20674, 5, null);
      var s__20681 = cljs.core.nth.call(null, vec__20671__20674, 6, null);
      var ms__20682 = cljs.core.nth.call(null, vec__20671__20674, 7, null);
      var vec__20672__20683 = cljs.core.nth.call(null, vec__20670__20673, 1, null);
      var offset_sign__20684 = cljs.core.nth.call(null, vec__20672__20683, 0, null);
      var offset_hours__20685 = cljs.core.nth.call(null, vec__20672__20683, 1, null);
      var offset_minutes__20686 = cljs.core.nth.call(null, vec__20672__20683, 2, null);
      var offset__20687 = offset_sign__20684 * (offset_hours__20685 * 60 + offset_minutes__20686);
      return cljs.core.PersistentVector.fromArray([cljs.core.not.call(null, years__20658) ? 1970 : y__20676, cljs.core.not.call(null, months__20659) ? 1 : check__20650.call(null, 1, mo__20677, 12, "timestamp month field must be in range 1..12"), cljs.core.not.call(null, days__20660) ? 1 : check__20650.call(null, 1, d__20678, cljs.reader.days_in_month.call(null, mo__20677, cljs.reader.leap_year_QMARK_.call(null, y__20676)), "timestamp day field must be in range 1..last day in month"), cljs.core.not.call(null, 
      hours__20661) ? 0 : check__20650.call(null, 0, h__20679, 23, "timestamp hour field must be in range 0..23"), cljs.core.not.call(null, minutes__20662) ? 0 : check__20650.call(null, 0, m__20680, 59, "timestamp minute field must be in range 0..59"), cljs.core.not.call(null, seconds__20663) ? 0 : check__20650.call(null, 0, s__20681, cljs.core._EQ_.call(null, m__20680, 59) ? 60 : 59, "timestamp second field must be in range 0..60"), cljs.core.not.call(null, milliseconds__20664) ? 0 : check__20650.call(null, 
      0, ms__20682, 999, "timestamp millisecond field must be in range 0..999"), offset__20687], true)
    }else {
      return null
    }
  }
}();
cljs.reader.parse_timestamp = function parse_timestamp(ts) {
  var temp__3971__auto____20699 = cljs.reader.parse_and_validate_timestamp.call(null, ts);
  if(cljs.core.truth_(temp__3971__auto____20699)) {
    var vec__20700__20701 = temp__3971__auto____20699;
    var years__20702 = cljs.core.nth.call(null, vec__20700__20701, 0, null);
    var months__20703 = cljs.core.nth.call(null, vec__20700__20701, 1, null);
    var days__20704 = cljs.core.nth.call(null, vec__20700__20701, 2, null);
    var hours__20705 = cljs.core.nth.call(null, vec__20700__20701, 3, null);
    var minutes__20706 = cljs.core.nth.call(null, vec__20700__20701, 4, null);
    var seconds__20707 = cljs.core.nth.call(null, vec__20700__20701, 5, null);
    var ms__20708 = cljs.core.nth.call(null, vec__20700__20701, 6, null);
    var offset__20709 = cljs.core.nth.call(null, vec__20700__20701, 7, null);
    return new Date(Date.UTC(years__20702, months__20703 - 1, days__20704, hours__20705, minutes__20706, seconds__20707, ms__20708) - offset__20709 * 60 * 1E3)
  }else {
    return cljs.reader.reader_error.call(null, null, [cljs.core.str("Unrecognized date/time syntax: "), cljs.core.str(ts)].join(""))
  }
};
cljs.reader.read_date = function read_date(s) {
  if(cljs.core.string_QMARK_.call(null, s)) {
    return cljs.reader.parse_timestamp.call(null, s)
  }else {
    return cljs.reader.reader_error.call(null, null, "Instance literal expects a string for its timestamp.")
  }
};
cljs.reader.read_queue = function read_queue(elems) {
  if(cljs.core.vector_QMARK_.call(null, elems)) {
    return cljs.core.into.call(null, cljs.core.PersistentQueue.EMPTY, elems)
  }else {
    return cljs.reader.reader_error.call(null, null, "Queue literal expects a vector for its elements.")
  }
};
cljs.reader.read_uuid = function read_uuid(uuid) {
  if(cljs.core.string_QMARK_.call(null, uuid)) {
    return new cljs.core.UUID(uuid)
  }else {
    return cljs.reader.reader_error.call(null, null, "UUID literal expects a string as its representation.")
  }
};
cljs.reader._STAR_tag_table_STAR_ = cljs.core.atom.call(null, cljs.core.ObjMap.fromObject(["inst", "uuid", "queue"], {"inst":cljs.reader.read_date, "uuid":cljs.reader.read_uuid, "queue":cljs.reader.read_queue}));
cljs.reader.maybe_read_tagged_type = function maybe_read_tagged_type(rdr, initch) {
  var tag__20713 = cljs.reader.read_symbol.call(null, rdr, initch);
  var temp__3971__auto____20714 = cljs.core._lookup.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_), cljs.core.name.call(null, tag__20713), null);
  if(cljs.core.truth_(temp__3971__auto____20714)) {
    var pfn__20715 = temp__3971__auto____20714;
    return pfn__20715.call(null, cljs.reader.read.call(null, rdr, true, null, false))
  }else {
    return cljs.reader.reader_error.call(null, rdr, "Could not find tag parser for ", cljs.core.name.call(null, tag__20713), " in ", cljs.core.pr_str.call(null, cljs.core.keys.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_))))
  }
};
cljs.reader.register_tag_parser_BANG_ = function register_tag_parser_BANG_(tag, f) {
  var tag__20718 = cljs.core.name.call(null, tag);
  var old_parser__20719 = cljs.core._lookup.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_), tag__20718, null);
  cljs.core.swap_BANG_.call(null, cljs.reader._STAR_tag_table_STAR_, cljs.core.assoc, tag__20718, f);
  return old_parser__20719
};
cljs.reader.deregister_tag_parser_BANG_ = function deregister_tag_parser_BANG_(tag) {
  var tag__20722 = cljs.core.name.call(null, tag);
  var old_parser__20723 = cljs.core._lookup.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_), tag__20722, null);
  cljs.core.swap_BANG_.call(null, cljs.reader._STAR_tag_table_STAR_, cljs.core.dissoc, tag__20722);
  return old_parser__20723
};
goog.provide("clojure.string");
goog.require("cljs.core");
goog.require("goog.string.StringBuffer");
goog.require("goog.string");
clojure.string.seq_reverse = function seq_reverse(coll) {
  return cljs.core.reduce.call(null, cljs.core.conj, cljs.core.List.EMPTY, coll)
};
clojure.string.reverse = function reverse(s) {
  return s.split("").reverse().join("")
};
clojure.string.replace = function replace(s, match, replacement) {
  if(cljs.core.string_QMARK_.call(null, match)) {
    return s.replace(new RegExp(goog.string.regExpEscape(match), "g"), replacement)
  }else {
    if(cljs.core.truth_(match.hasOwnProperty("source"))) {
      return s.replace(new RegExp(match.source, "g"), replacement)
    }else {
      if("\ufdd0'else") {
        throw[cljs.core.str("Invalid match arg: "), cljs.core.str(match)].join("");
      }else {
        return null
      }
    }
  }
};
clojure.string.replace_first = function replace_first(s, match, replacement) {
  return s.replace(match, replacement)
};
clojure.string.join = function() {
  var join = null;
  var join__1 = function(coll) {
    return cljs.core.apply.call(null, cljs.core.str, coll)
  };
  var join__2 = function(separator, coll) {
    return cljs.core.apply.call(null, cljs.core.str, cljs.core.interpose.call(null, separator, coll))
  };
  join = function(separator, coll) {
    switch(arguments.length) {
      case 1:
        return join__1.call(this, separator);
      case 2:
        return join__2.call(this, separator, coll)
    }
    throw"Invalid arity: " + arguments.length;
  };
  join.cljs$lang$arity$1 = join__1;
  join.cljs$lang$arity$2 = join__2;
  return join
}();
clojure.string.upper_case = function upper_case(s) {
  return s.toUpperCase()
};
clojure.string.lower_case = function lower_case(s) {
  return s.toLowerCase()
};
clojure.string.capitalize = function capitalize(s) {
  if(cljs.core.count.call(null, s) < 2) {
    return clojure.string.upper_case.call(null, s)
  }else {
    return[cljs.core.str(clojure.string.upper_case.call(null, cljs.core.subs.call(null, s, 0, 1))), cljs.core.str(clojure.string.lower_case.call(null, cljs.core.subs.call(null, s, 1)))].join("")
  }
};
clojure.string.split = function() {
  var split = null;
  var split__2 = function(s, re) {
    return cljs.core.vec.call(null, [cljs.core.str(s)].join("").split(re))
  };
  var split__3 = function(s, re, limit) {
    if(limit < 1) {
      return cljs.core.vec.call(null, [cljs.core.str(s)].join("").split(re))
    }else {
      var s__20730 = s;
      var limit__20731 = limit;
      var parts__20732 = cljs.core.PersistentVector.EMPTY;
      while(true) {
        if(cljs.core._EQ_.call(null, limit__20731, 1)) {
          return cljs.core.conj.call(null, parts__20732, s__20730)
        }else {
          var temp__3971__auto____20733 = cljs.core.re_find.call(null, re, s__20730);
          if(cljs.core.truth_(temp__3971__auto____20733)) {
            var m__20734 = temp__3971__auto____20733;
            var index__20735 = s__20730.indexOf(m__20734);
            var G__20736 = s__20730.substring(index__20735 + cljs.core.count.call(null, m__20734));
            var G__20737 = limit__20731 - 1;
            var G__20738 = cljs.core.conj.call(null, parts__20732, s__20730.substring(0, index__20735));
            s__20730 = G__20736;
            limit__20731 = G__20737;
            parts__20732 = G__20738;
            continue
          }else {
            return cljs.core.conj.call(null, parts__20732, s__20730)
          }
        }
        break
      }
    }
  };
  split = function(s, re, limit) {
    switch(arguments.length) {
      case 2:
        return split__2.call(this, s, re);
      case 3:
        return split__3.call(this, s, re, limit)
    }
    throw"Invalid arity: " + arguments.length;
  };
  split.cljs$lang$arity$2 = split__2;
  split.cljs$lang$arity$3 = split__3;
  return split
}();
clojure.string.split_lines = function split_lines(s) {
  return clojure.string.split.call(null, s, /\n|\r\n/)
};
clojure.string.trim = function trim(s) {
  return goog.string.trim(s)
};
clojure.string.triml = function triml(s) {
  return goog.string.trimLeft(s)
};
clojure.string.trimr = function trimr(s) {
  return goog.string.trimRight(s)
};
clojure.string.trim_newline = function trim_newline(s) {
  var index__20742 = s.length;
  while(true) {
    if(index__20742 === 0) {
      return""
    }else {
      var ch__20743 = cljs.core._lookup.call(null, s, index__20742 - 1, null);
      if(function() {
        var or__3824__auto____20744 = cljs.core._EQ_.call(null, ch__20743, "\n");
        if(or__3824__auto____20744) {
          return or__3824__auto____20744
        }else {
          return cljs.core._EQ_.call(null, ch__20743, "\r")
        }
      }()) {
        var G__20745 = index__20742 - 1;
        index__20742 = G__20745;
        continue
      }else {
        return s.substring(0, index__20742)
      }
    }
    break
  }
};
clojure.string.blank_QMARK_ = function blank_QMARK_(s) {
  return goog.string.isEmptySafe(s)
};
clojure.string.escape = function escape(s, cmap) {
  var buffer__20752 = new goog.string.StringBuffer;
  var length__20753 = s.length;
  var index__20754 = 0;
  while(true) {
    if(cljs.core._EQ_.call(null, length__20753, index__20754)) {
      return buffer__20752.toString()
    }else {
      var ch__20755 = s.charAt(index__20754);
      var temp__3971__auto____20756 = cljs.core._lookup.call(null, cmap, ch__20755, null);
      if(cljs.core.truth_(temp__3971__auto____20756)) {
        var replacement__20757 = temp__3971__auto____20756;
        buffer__20752.append([cljs.core.str(replacement__20757)].join(""))
      }else {
        buffer__20752.append(ch__20755)
      }
      var G__20758 = index__20754 + 1;
      index__20754 = G__20758;
      continue
    }
    break
  }
};
goog.provide("jayq.core");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("jayq.util");
goog.require("cljs.reader");
goog.require("clojure.string");
jayq.core.crate_meta = function crate_meta(func) {
  return func.prototype._crateGroup
};
jayq.core.__GT_selector = function __GT_selector(sel) {
  if(cljs.core.string_QMARK_.call(null, sel)) {
    return sel
  }else {
    if(cljs.core.fn_QMARK_.call(null, sel)) {
      var temp__3971__auto____20271 = jayq.core.crate_meta.call(null, sel);
      if(cljs.core.truth_(temp__3971__auto____20271)) {
        var cm__20272 = temp__3971__auto____20271;
        return[cljs.core.str("[crateGroup="), cljs.core.str(cm__20272), cljs.core.str("]")].join("")
      }else {
        return sel
      }
    }else {
      if(cljs.core.keyword_QMARK_.call(null, sel)) {
        return cljs.core.name.call(null, sel)
      }else {
        if("\ufdd0'else") {
          return sel
        }else {
          return null
        }
      }
    }
  }
};
jayq.core.$ = function() {
  var $__delegate = function(sel, p__20273) {
    var vec__20277__20278 = p__20273;
    var context__20279 = cljs.core.nth.call(null, vec__20277__20278, 0, null);
    if(cljs.core.not.call(null, context__20279)) {
      return jQuery(jayq.core.__GT_selector.call(null, sel))
    }else {
      return jQuery(jayq.core.__GT_selector.call(null, sel), context__20279)
    }
  };
  var $ = function(sel, var_args) {
    var p__20273 = null;
    if(goog.isDef(var_args)) {
      p__20273 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return $__delegate.call(this, sel, p__20273)
  };
  $.cljs$lang$maxFixedArity = 1;
  $.cljs$lang$applyTo = function(arglist__20280) {
    var sel = cljs.core.first(arglist__20280);
    var p__20273 = cljs.core.rest(arglist__20280);
    return $__delegate(sel, p__20273)
  };
  $.cljs$lang$arity$variadic = $__delegate;
  return $
}();
jQuery.prototype.cljs$core$IReduce$ = true;
jQuery.prototype.cljs$core$IReduce$_reduce$arity$2 = function(this$, f) {
  return cljs.core.ci_reduce.call(null, this$, f)
};
jQuery.prototype.cljs$core$IReduce$_reduce$arity$3 = function(this$, f, start) {
  return cljs.core.ci_reduce.call(null, this$, f, start)
};
jQuery.prototype.cljs$core$ILookup$ = true;
jQuery.prototype.cljs$core$ILookup$_lookup$arity$2 = function(this$, k) {
  var or__3824__auto____20281 = this$.slice(k, k + 1);
  if(cljs.core.truth_(or__3824__auto____20281)) {
    return or__3824__auto____20281
  }else {
    return null
  }
};
jQuery.prototype.cljs$core$ILookup$_lookup$arity$3 = function(this$, k, not_found) {
  return cljs.core._nth.call(null, this$, k, not_found)
};
jQuery.prototype.cljs$core$ISequential$ = true;
jQuery.prototype.cljs$core$IIndexed$ = true;
jQuery.prototype.cljs$core$IIndexed$_nth$arity$2 = function(this$, n) {
  if(n < cljs.core.count.call(null, this$)) {
    return this$.slice(n, n + 1)
  }else {
    return null
  }
};
jQuery.prototype.cljs$core$IIndexed$_nth$arity$3 = function(this$, n, not_found) {
  if(n < cljs.core.count.call(null, this$)) {
    return this$.slice(n, n + 1)
  }else {
    if(void 0 === not_found) {
      return null
    }else {
      return not_found
    }
  }
};
jQuery.prototype.cljs$core$ICounted$ = true;
jQuery.prototype.cljs$core$ICounted$_count$arity$1 = function(this$) {
  return this$.size()
};
jQuery.prototype.cljs$core$ISeq$ = true;
jQuery.prototype.cljs$core$ISeq$_first$arity$1 = function(this$) {
  return this$.get(0)
};
jQuery.prototype.cljs$core$ISeq$_rest$arity$1 = function(this$) {
  if(cljs.core.count.call(null, this$) > 1) {
    return this$.slice(1)
  }else {
    return cljs.core.list.call(null)
  }
};
jQuery.prototype.cljs$core$ISeqable$ = true;
jQuery.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  if(cljs.core.truth_(this$.get(0))) {
    return this$
  }else {
    return null
  }
};
jQuery.prototype.call = function() {
  var G__20282 = null;
  var G__20282__2 = function(_, k) {
    return cljs.core._lookup.call(null, this, k)
  };
  var G__20282__3 = function(_, k, not_found) {
    return cljs.core._lookup.call(null, this, k, not_found)
  };
  G__20282 = function(_, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__20282__2.call(this, _, k);
      case 3:
        return G__20282__3.call(this, _, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__20282
}();
jayq.core.anim = function anim(elem, props, dur) {
  return elem.animate(jayq.util.clj__GT_js.call(null, props), dur)
};
jayq.core.text = function() {
  var text = null;
  var text__1 = function($elem) {
    return $elem.text()
  };
  var text__2 = function($elem, txt) {
    return $elem.text(txt)
  };
  text = function($elem, txt) {
    switch(arguments.length) {
      case 1:
        return text__1.call(this, $elem);
      case 2:
        return text__2.call(this, $elem, txt)
    }
    throw"Invalid arity: " + arguments.length;
  };
  text.cljs$lang$arity$1 = text__1;
  text.cljs$lang$arity$2 = text__2;
  return text
}();
jayq.core.css = function css($elem, opts) {
  if(cljs.core.keyword_QMARK_.call(null, opts)) {
    return $elem.css(cljs.core.name.call(null, opts))
  }else {
    return $elem.css(jayq.util.clj__GT_js.call(null, opts))
  }
};
jayq.core.attr = function() {
  var attr__delegate = function($elem, a, p__20283) {
    var vec__20288__20289 = p__20283;
    var v__20290 = cljs.core.nth.call(null, vec__20288__20289, 0, null);
    var a__20291 = cljs.core.name.call(null, a);
    if(cljs.core.not.call(null, v__20290)) {
      return $elem.attr(a__20291)
    }else {
      return $elem.attr(a__20291, v__20290)
    }
  };
  var attr = function($elem, a, var_args) {
    var p__20283 = null;
    if(goog.isDef(var_args)) {
      p__20283 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return attr__delegate.call(this, $elem, a, p__20283)
  };
  attr.cljs$lang$maxFixedArity = 2;
  attr.cljs$lang$applyTo = function(arglist__20292) {
    var $elem = cljs.core.first(arglist__20292);
    var a = cljs.core.first(cljs.core.next(arglist__20292));
    var p__20283 = cljs.core.rest(cljs.core.next(arglist__20292));
    return attr__delegate($elem, a, p__20283)
  };
  attr.cljs$lang$arity$variadic = attr__delegate;
  return attr
}();
jayq.core.remove_attr = function remove_attr($elem, a) {
  return $elem.removeAttr(cljs.core.name.call(null, a))
};
jayq.core.data = function() {
  var data__delegate = function($elem, k, p__20293) {
    var vec__20298__20299 = p__20293;
    var v__20300 = cljs.core.nth.call(null, vec__20298__20299, 0, null);
    var k__20301 = cljs.core.name.call(null, k);
    if(cljs.core.not.call(null, v__20300)) {
      return $elem.data(k__20301)
    }else {
      return $elem.data(k__20301, v__20300)
    }
  };
  var data = function($elem, k, var_args) {
    var p__20293 = null;
    if(goog.isDef(var_args)) {
      p__20293 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return data__delegate.call(this, $elem, k, p__20293)
  };
  data.cljs$lang$maxFixedArity = 2;
  data.cljs$lang$applyTo = function(arglist__20302) {
    var $elem = cljs.core.first(arglist__20302);
    var k = cljs.core.first(cljs.core.next(arglist__20302));
    var p__20293 = cljs.core.rest(cljs.core.next(arglist__20302));
    return data__delegate($elem, k, p__20293)
  };
  data.cljs$lang$arity$variadic = data__delegate;
  return data
}();
jayq.core.position = function position($elem) {
  return cljs.core.js__GT_clj.call(null, $elem.position(), "\ufdd0'keywordize-keys", true)
};
jayq.core.add_class = function add_class($elem, cl) {
  var cl__20304 = cljs.core.name.call(null, cl);
  return $elem.addClass(cl__20304)
};
jayq.core.remove_class = function remove_class($elem, cl) {
  var cl__20306 = cljs.core.name.call(null, cl);
  return $elem.removeClass(cl__20306)
};
jayq.core.toggle_class = function toggle_class($elem, cl) {
  var cl__20308 = cljs.core.name.call(null, cl);
  return $elem.toggleClass(cl__20308)
};
jayq.core.has_class = function has_class($elem, cl) {
  var cl__20310 = cljs.core.name.call(null, cl);
  return $elem.hasClass(cl__20310)
};
jayq.core.is = function is($elem, selector) {
  return $elem.is(jayq.core.__GT_selector.call(null, selector))
};
jayq.core.after = function after($elem, content) {
  return $elem.after(content)
};
jayq.core.before = function before($elem, content) {
  return $elem.before(content)
};
jayq.core.append = function append($elem, content) {
  return $elem.append(content)
};
jayq.core.prepend = function prepend($elem, content) {
  return $elem.prepend(content)
};
jayq.core.append_to = function append_to($elem, target) {
  return $elem.appendTo(jayq.core.__GT_selector.call(null, target))
};
jayq.core.prepend_to = function prepend_to($elem, target) {
  return $elem.prependTo(jayq.core.__GT_selector.call(null, target))
};
jayq.core.insert_before = function insert_before($elem, target) {
  return $elem.insertBefore(jayq.core.__GT_selector.call(null, target))
};
jayq.core.insert_after = function insert_after($elem, target) {
  return $elem.insertAfter(jayq.core.__GT_selector.call(null, target))
};
jayq.core.remove = function remove($elem) {
  return $elem.remove()
};
jayq.core.hide = function() {
  var hide__delegate = function($elem, p__20311) {
    var vec__20316__20317 = p__20311;
    var speed__20318 = cljs.core.nth.call(null, vec__20316__20317, 0, null);
    var on_finish__20319 = cljs.core.nth.call(null, vec__20316__20317, 1, null);
    return $elem.hide(speed__20318, on_finish__20319)
  };
  var hide = function($elem, var_args) {
    var p__20311 = null;
    if(goog.isDef(var_args)) {
      p__20311 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return hide__delegate.call(this, $elem, p__20311)
  };
  hide.cljs$lang$maxFixedArity = 1;
  hide.cljs$lang$applyTo = function(arglist__20320) {
    var $elem = cljs.core.first(arglist__20320);
    var p__20311 = cljs.core.rest(arglist__20320);
    return hide__delegate($elem, p__20311)
  };
  hide.cljs$lang$arity$variadic = hide__delegate;
  return hide
}();
jayq.core.show = function() {
  var show__delegate = function($elem, p__20321) {
    var vec__20326__20327 = p__20321;
    var speed__20328 = cljs.core.nth.call(null, vec__20326__20327, 0, null);
    var on_finish__20329 = cljs.core.nth.call(null, vec__20326__20327, 1, null);
    return $elem.show(speed__20328, on_finish__20329)
  };
  var show = function($elem, var_args) {
    var p__20321 = null;
    if(goog.isDef(var_args)) {
      p__20321 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return show__delegate.call(this, $elem, p__20321)
  };
  show.cljs$lang$maxFixedArity = 1;
  show.cljs$lang$applyTo = function(arglist__20330) {
    var $elem = cljs.core.first(arglist__20330);
    var p__20321 = cljs.core.rest(arglist__20330);
    return show__delegate($elem, p__20321)
  };
  show.cljs$lang$arity$variadic = show__delegate;
  return show
}();
jayq.core.toggle = function() {
  var toggle__delegate = function($elem, p__20331) {
    var vec__20336__20337 = p__20331;
    var speed__20338 = cljs.core.nth.call(null, vec__20336__20337, 0, null);
    var on_finish__20339 = cljs.core.nth.call(null, vec__20336__20337, 1, null);
    return $elem.toggle(speed__20338, on_finish__20339)
  };
  var toggle = function($elem, var_args) {
    var p__20331 = null;
    if(goog.isDef(var_args)) {
      p__20331 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return toggle__delegate.call(this, $elem, p__20331)
  };
  toggle.cljs$lang$maxFixedArity = 1;
  toggle.cljs$lang$applyTo = function(arglist__20340) {
    var $elem = cljs.core.first(arglist__20340);
    var p__20331 = cljs.core.rest(arglist__20340);
    return toggle__delegate($elem, p__20331)
  };
  toggle.cljs$lang$arity$variadic = toggle__delegate;
  return toggle
}();
jayq.core.fade_out = function() {
  var fade_out__delegate = function($elem, p__20341) {
    var vec__20346__20347 = p__20341;
    var speed__20348 = cljs.core.nth.call(null, vec__20346__20347, 0, null);
    var on_finish__20349 = cljs.core.nth.call(null, vec__20346__20347, 1, null);
    return $elem.fadeOut(speed__20348, on_finish__20349)
  };
  var fade_out = function($elem, var_args) {
    var p__20341 = null;
    if(goog.isDef(var_args)) {
      p__20341 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return fade_out__delegate.call(this, $elem, p__20341)
  };
  fade_out.cljs$lang$maxFixedArity = 1;
  fade_out.cljs$lang$applyTo = function(arglist__20350) {
    var $elem = cljs.core.first(arglist__20350);
    var p__20341 = cljs.core.rest(arglist__20350);
    return fade_out__delegate($elem, p__20341)
  };
  fade_out.cljs$lang$arity$variadic = fade_out__delegate;
  return fade_out
}();
jayq.core.fade_in = function() {
  var fade_in__delegate = function($elem, p__20351) {
    var vec__20356__20357 = p__20351;
    var speed__20358 = cljs.core.nth.call(null, vec__20356__20357, 0, null);
    var on_finish__20359 = cljs.core.nth.call(null, vec__20356__20357, 1, null);
    return $elem.fadeIn(speed__20358, on_finish__20359)
  };
  var fade_in = function($elem, var_args) {
    var p__20351 = null;
    if(goog.isDef(var_args)) {
      p__20351 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return fade_in__delegate.call(this, $elem, p__20351)
  };
  fade_in.cljs$lang$maxFixedArity = 1;
  fade_in.cljs$lang$applyTo = function(arglist__20360) {
    var $elem = cljs.core.first(arglist__20360);
    var p__20351 = cljs.core.rest(arglist__20360);
    return fade_in__delegate($elem, p__20351)
  };
  fade_in.cljs$lang$arity$variadic = fade_in__delegate;
  return fade_in
}();
jayq.core.slide_up = function() {
  var slide_up__delegate = function($elem, p__20361) {
    var vec__20366__20367 = p__20361;
    var speed__20368 = cljs.core.nth.call(null, vec__20366__20367, 0, null);
    var on_finish__20369 = cljs.core.nth.call(null, vec__20366__20367, 1, null);
    return $elem.slideUp(speed__20368, on_finish__20369)
  };
  var slide_up = function($elem, var_args) {
    var p__20361 = null;
    if(goog.isDef(var_args)) {
      p__20361 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return slide_up__delegate.call(this, $elem, p__20361)
  };
  slide_up.cljs$lang$maxFixedArity = 1;
  slide_up.cljs$lang$applyTo = function(arglist__20370) {
    var $elem = cljs.core.first(arglist__20370);
    var p__20361 = cljs.core.rest(arglist__20370);
    return slide_up__delegate($elem, p__20361)
  };
  slide_up.cljs$lang$arity$variadic = slide_up__delegate;
  return slide_up
}();
jayq.core.slide_down = function() {
  var slide_down__delegate = function($elem, p__20371) {
    var vec__20376__20377 = p__20371;
    var speed__20378 = cljs.core.nth.call(null, vec__20376__20377, 0, null);
    var on_finish__20379 = cljs.core.nth.call(null, vec__20376__20377, 1, null);
    return $elem.slideDown(speed__20378, on_finish__20379)
  };
  var slide_down = function($elem, var_args) {
    var p__20371 = null;
    if(goog.isDef(var_args)) {
      p__20371 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return slide_down__delegate.call(this, $elem, p__20371)
  };
  slide_down.cljs$lang$maxFixedArity = 1;
  slide_down.cljs$lang$applyTo = function(arglist__20380) {
    var $elem = cljs.core.first(arglist__20380);
    var p__20371 = cljs.core.rest(arglist__20380);
    return slide_down__delegate($elem, p__20371)
  };
  slide_down.cljs$lang$arity$variadic = slide_down__delegate;
  return slide_down
}();
jayq.core.siblings = function() {
  var siblings = null;
  var siblings__1 = function($elem) {
    return $elem.siblings()
  };
  var siblings__2 = function($elem, selector) {
    return $elem.siblings(cljs.core.name.call(null, selector))
  };
  siblings = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return siblings__1.call(this, $elem);
      case 2:
        return siblings__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  siblings.cljs$lang$arity$1 = siblings__1;
  siblings.cljs$lang$arity$2 = siblings__2;
  return siblings
}();
jayq.core.parent = function parent($elem) {
  return $elem.parent()
};
jayq.core.parents = function() {
  var parents = null;
  var parents__1 = function($elem) {
    return $elem.parents()
  };
  var parents__2 = function($elem, selector) {
    return $elem.parents(cljs.core.name.call(null, selector))
  };
  parents = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return parents__1.call(this, $elem);
      case 2:
        return parents__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  parents.cljs$lang$arity$1 = parents__1;
  parents.cljs$lang$arity$2 = parents__2;
  return parents
}();
jayq.core.parents_until = function() {
  var parents_until = null;
  var parents_until__1 = function($elem) {
    return $elem.parentsUntil()
  };
  var parents_until__2 = function($elem, selector) {
    return $elem.parentsUntil(jayq.core.__GT_selector.call(null, selector))
  };
  var parents_until__3 = function($elem, selector, filtr) {
    return $elem.parentsUntil(jayq.core.__GT_selector.call(null, selector), cljs.core.name.call(null, filtr))
  };
  parents_until = function($elem, selector, filtr) {
    switch(arguments.length) {
      case 1:
        return parents_until__1.call(this, $elem);
      case 2:
        return parents_until__2.call(this, $elem, selector);
      case 3:
        return parents_until__3.call(this, $elem, selector, filtr)
    }
    throw"Invalid arity: " + arguments.length;
  };
  parents_until.cljs$lang$arity$1 = parents_until__1;
  parents_until.cljs$lang$arity$2 = parents_until__2;
  parents_until.cljs$lang$arity$3 = parents_until__3;
  return parents_until
}();
jayq.core.children = function() {
  var children = null;
  var children__1 = function($elem) {
    return $elem.children()
  };
  var children__2 = function($elem, selector) {
    return $elem.children(cljs.core.name.call(null, selector))
  };
  children = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return children__1.call(this, $elem);
      case 2:
        return children__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  children.cljs$lang$arity$1 = children__1;
  children.cljs$lang$arity$2 = children__2;
  return children
}();
jayq.core.next = function() {
  var next = null;
  var next__1 = function($elem) {
    return $elem.next()
  };
  var next__2 = function($elem, selector) {
    return $elem.next(cljs.core.name.call(null, selector))
  };
  next = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return next__1.call(this, $elem);
      case 2:
        return next__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  next.cljs$lang$arity$1 = next__1;
  next.cljs$lang$arity$2 = next__2;
  return next
}();
jayq.core.prev = function() {
  var prev = null;
  var prev__1 = function($elem) {
    return $elem.prev()
  };
  var prev__2 = function($elem, selector) {
    return $elem.prev(cljs.core.name.call(null, selector))
  };
  prev = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return prev__1.call(this, $elem);
      case 2:
        return prev__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  prev.cljs$lang$arity$1 = prev__1;
  prev.cljs$lang$arity$2 = prev__2;
  return prev
}();
jayq.core.next_all = function() {
  var next_all = null;
  var next_all__1 = function($elem) {
    return $elem.nextAll()
  };
  var next_all__2 = function($elem, selector) {
    return $elem.nextAll(cljs.core.name.call(null, selector))
  };
  next_all = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return next_all__1.call(this, $elem);
      case 2:
        return next_all__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  next_all.cljs$lang$arity$1 = next_all__1;
  next_all.cljs$lang$arity$2 = next_all__2;
  return next_all
}();
jayq.core.prev_all = function() {
  var prev_all = null;
  var prev_all__1 = function($elem) {
    return $elem.prevAll()
  };
  var prev_all__2 = function($elem, selector) {
    return $elem.prevAll(cljs.core.name.call(null, selector))
  };
  prev_all = function($elem, selector) {
    switch(arguments.length) {
      case 1:
        return prev_all__1.call(this, $elem);
      case 2:
        return prev_all__2.call(this, $elem, selector)
    }
    throw"Invalid arity: " + arguments.length;
  };
  prev_all.cljs$lang$arity$1 = prev_all__1;
  prev_all.cljs$lang$arity$2 = prev_all__2;
  return prev_all
}();
jayq.core.next_until = function() {
  var next_until = null;
  var next_until__1 = function($elem) {
    return $elem.nextUntil()
  };
  var next_until__2 = function($elem, selector) {
    return $elem.nextUntil(jayq.core.__GT_selector.call(null, selector))
  };
  var next_until__3 = function($elem, selector, filtr) {
    return $elem.nextUntil(jayq.core.__GT_selector.call(null, selector), cljs.core.name.call(null, filtr))
  };
  next_until = function($elem, selector, filtr) {
    switch(arguments.length) {
      case 1:
        return next_until__1.call(this, $elem);
      case 2:
        return next_until__2.call(this, $elem, selector);
      case 3:
        return next_until__3.call(this, $elem, selector, filtr)
    }
    throw"Invalid arity: " + arguments.length;
  };
  next_until.cljs$lang$arity$1 = next_until__1;
  next_until.cljs$lang$arity$2 = next_until__2;
  next_until.cljs$lang$arity$3 = next_until__3;
  return next_until
}();
jayq.core.prev_until = function() {
  var prev_until = null;
  var prev_until__1 = function($elem) {
    return $elem.prevUntil()
  };
  var prev_until__2 = function($elem, selector) {
    return $elem.prevUntil(jayq.core.__GT_selector.call(null, selector))
  };
  var prev_until__3 = function($elem, selector, filtr) {
    return $elem.prevUntil(jayq.core.__GT_selector.call(null, selector), cljs.core.name.call(null, filtr))
  };
  prev_until = function($elem, selector, filtr) {
    switch(arguments.length) {
      case 1:
        return prev_until__1.call(this, $elem);
      case 2:
        return prev_until__2.call(this, $elem, selector);
      case 3:
        return prev_until__3.call(this, $elem, selector, filtr)
    }
    throw"Invalid arity: " + arguments.length;
  };
  prev_until.cljs$lang$arity$1 = prev_until__1;
  prev_until.cljs$lang$arity$2 = prev_until__2;
  prev_until.cljs$lang$arity$3 = prev_until__3;
  return prev_until
}();
jayq.core.find = function find($elem, selector) {
  return $elem.find(cljs.core.name.call(null, selector))
};
jayq.core.closest = function() {
  var closest__delegate = function($elem, selector, p__20381) {
    var vec__20385__20386 = p__20381;
    var context__20387 = cljs.core.nth.call(null, vec__20385__20386, 0, null);
    return $elem.closest(jayq.core.__GT_selector.call(null, selector), context__20387)
  };
  var closest = function($elem, selector, var_args) {
    var p__20381 = null;
    if(goog.isDef(var_args)) {
      p__20381 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return closest__delegate.call(this, $elem, selector, p__20381)
  };
  closest.cljs$lang$maxFixedArity = 2;
  closest.cljs$lang$applyTo = function(arglist__20388) {
    var $elem = cljs.core.first(arglist__20388);
    var selector = cljs.core.first(cljs.core.next(arglist__20388));
    var p__20381 = cljs.core.rest(cljs.core.next(arglist__20388));
    return closest__delegate($elem, selector, p__20381)
  };
  closest.cljs$lang$arity$variadic = closest__delegate;
  return closest
}();
jayq.core.clone = function clone($elem) {
  return $elem.clone()
};
jayq.core.inner = function inner($elem, v) {
  return $elem.html(v)
};
jayq.core.empty = function empty($elem) {
  return $elem.empty()
};
jayq.core.val = function() {
  var val__delegate = function($elem, p__20389) {
    var vec__20393__20394 = p__20389;
    var v__20395 = cljs.core.nth.call(null, vec__20393__20394, 0, null);
    if(cljs.core.truth_(v__20395)) {
      return $elem.val(v__20395)
    }else {
      return $elem.val()
    }
  };
  var val = function($elem, var_args) {
    var p__20389 = null;
    if(goog.isDef(var_args)) {
      p__20389 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return val__delegate.call(this, $elem, p__20389)
  };
  val.cljs$lang$maxFixedArity = 1;
  val.cljs$lang$applyTo = function(arglist__20396) {
    var $elem = cljs.core.first(arglist__20396);
    var p__20389 = cljs.core.rest(arglist__20396);
    return val__delegate($elem, p__20389)
  };
  val.cljs$lang$arity$variadic = val__delegate;
  return val
}();
jayq.core.serialize = function serialize($elem) {
  return $elem.serialize()
};
jayq.core.queue = function queue($elem, callback) {
  return $elem.queue(callback)
};
jayq.core.dequeue = function dequeue(elem) {
  return jayq.core.$.call(null, elem).dequeue()
};
jayq.core.document_ready = function document_ready(func) {
  return jayq.core.$.call(null, document).ready(func)
};
jayq.core.xhr = function xhr(p__20397, content, callback) {
  var vec__20403__20404 = p__20397;
  var method__20405 = cljs.core.nth.call(null, vec__20403__20404, 0, null);
  var uri__20406 = cljs.core.nth.call(null, vec__20403__20404, 1, null);
  var params__20407 = jayq.util.clj__GT_js.call(null, cljs.core.ObjMap.fromObject(["\ufdd0'type", "\ufdd0'data", "\ufdd0'success"], {"\ufdd0'type":clojure.string.upper_case.call(null, cljs.core.name.call(null, method__20405)), "\ufdd0'data":jayq.util.clj__GT_js.call(null, content), "\ufdd0'success":callback}));
  return jQuery.ajax(uri__20406, params__20407)
};
jayq.core.ajax = function() {
  var ajax = null;
  var ajax__1 = function(settings) {
    return jQuery.ajax(jayq.util.clj__GT_js.call(null, settings))
  };
  var ajax__2 = function(url, settings) {
    return jQuery.ajax(url, jayq.util.clj__GT_js.call(null, settings))
  };
  ajax = function(url, settings) {
    switch(arguments.length) {
      case 1:
        return ajax__1.call(this, url);
      case 2:
        return ajax__2.call(this, url, settings)
    }
    throw"Invalid arity: " + arguments.length;
  };
  ajax.cljs$lang$arity$1 = ajax__1;
  ajax.cljs$lang$arity$2 = ajax__2;
  return ajax
}();
jayq.core.mimetype_converter = function mimetype_converter(s) {
  return cljs.reader.read_string.call(null, [cljs.core.str(s)].join(""))
};
jQuery.ajaxSetup(jayq.util.clj__GT_js.call(null, cljs.core.ObjMap.fromObject(["\ufdd0'accepts", "\ufdd0'contents", "\ufdd0'converters"], {"\ufdd0'accepts":cljs.core.ObjMap.fromObject(["\ufdd0'edn", "\ufdd0'clojure"], {"\ufdd0'edn":"application/edn, text/edn", "\ufdd0'clojure":"application/clojure, text/clojure"}), "\ufdd0'contents":cljs.core.ObjMap.fromObject(["clojure"], {"clojure":/edn|clojure/}), "\ufdd0'converters":cljs.core.ObjMap.fromObject(["text edn", "text clojure"], {"text edn":jayq.core.mimetype_converter, 
"text clojure":jayq.core.mimetype_converter})})));
jayq.core.bind = function bind($elem, ev, func) {
  return $elem.bind(cljs.core.name.call(null, ev), func)
};
jayq.core.unbind = function() {
  var unbind__delegate = function($elem, ev, p__20408) {
    var vec__20412__20413 = p__20408;
    var func__20414 = cljs.core.nth.call(null, vec__20412__20413, 0, null);
    return $elem.unbind(cljs.core.name.call(null, ev), func__20414)
  };
  var unbind = function($elem, ev, var_args) {
    var p__20408 = null;
    if(goog.isDef(var_args)) {
      p__20408 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return unbind__delegate.call(this, $elem, ev, p__20408)
  };
  unbind.cljs$lang$maxFixedArity = 2;
  unbind.cljs$lang$applyTo = function(arglist__20415) {
    var $elem = cljs.core.first(arglist__20415);
    var ev = cljs.core.first(cljs.core.next(arglist__20415));
    var p__20408 = cljs.core.rest(cljs.core.next(arglist__20415));
    return unbind__delegate($elem, ev, p__20408)
  };
  unbind.cljs$lang$arity$variadic = unbind__delegate;
  return unbind
}();
jayq.core.trigger = function trigger($elem, ev) {
  return $elem.trigger(cljs.core.name.call(null, ev))
};
jayq.core.delegate = function delegate($elem, sel, ev, func) {
  return $elem.delegate(jayq.core.__GT_selector.call(null, sel), cljs.core.name.call(null, ev), func)
};
jayq.core.__GT_event = function __GT_event(e) {
  if(cljs.core.keyword_QMARK_.call(null, e)) {
    return cljs.core.name.call(null, e)
  }else {
    if(cljs.core.map_QMARK_.call(null, e)) {
      return jayq.util.clj__GT_js.call(null, e)
    }else {
      if(cljs.core.sequential_QMARK_.call(null, e)) {
        return clojure.string.join.call(null, " ", cljs.core.map.call(null, cljs.core.name, e))
      }else {
        if("\ufdd0'else") {
          throw new Error([cljs.core.str("Unknown event type: "), cljs.core.str(e)].join(""));
        }else {
          return null
        }
      }
    }
  }
};
jayq.core.on = function() {
  var on__delegate = function($elem, events, p__20416) {
    var vec__20422__20423 = p__20416;
    var sel__20424 = cljs.core.nth.call(null, vec__20422__20423, 0, null);
    var data__20425 = cljs.core.nth.call(null, vec__20422__20423, 1, null);
    var handler__20426 = cljs.core.nth.call(null, vec__20422__20423, 2, null);
    return $elem.on(jayq.core.__GT_event.call(null, events), jayq.core.__GT_selector.call(null, sel__20424), data__20425, handler__20426)
  };
  var on = function($elem, events, var_args) {
    var p__20416 = null;
    if(goog.isDef(var_args)) {
      p__20416 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return on__delegate.call(this, $elem, events, p__20416)
  };
  on.cljs$lang$maxFixedArity = 2;
  on.cljs$lang$applyTo = function(arglist__20427) {
    var $elem = cljs.core.first(arglist__20427);
    var events = cljs.core.first(cljs.core.next(arglist__20427));
    var p__20416 = cljs.core.rest(cljs.core.next(arglist__20427));
    return on__delegate($elem, events, p__20416)
  };
  on.cljs$lang$arity$variadic = on__delegate;
  return on
}();
jayq.core.one = function() {
  var one__delegate = function($elem, events, p__20428) {
    var vec__20434__20435 = p__20428;
    var sel__20436 = cljs.core.nth.call(null, vec__20434__20435, 0, null);
    var data__20437 = cljs.core.nth.call(null, vec__20434__20435, 1, null);
    var handler__20438 = cljs.core.nth.call(null, vec__20434__20435, 2, null);
    return $elem.one(jayq.core.__GT_event.call(null, events), jayq.core.__GT_selector.call(null, sel__20436), data__20437, handler__20438)
  };
  var one = function($elem, events, var_args) {
    var p__20428 = null;
    if(goog.isDef(var_args)) {
      p__20428 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return one__delegate.call(this, $elem, events, p__20428)
  };
  one.cljs$lang$maxFixedArity = 2;
  one.cljs$lang$applyTo = function(arglist__20439) {
    var $elem = cljs.core.first(arglist__20439);
    var events = cljs.core.first(cljs.core.next(arglist__20439));
    var p__20428 = cljs.core.rest(cljs.core.next(arglist__20439));
    return one__delegate($elem, events, p__20428)
  };
  one.cljs$lang$arity$variadic = one__delegate;
  return one
}();
jayq.core.off = function() {
  var off__delegate = function($elem, events, p__20440) {
    var vec__20445__20446 = p__20440;
    var sel__20447 = cljs.core.nth.call(null, vec__20445__20446, 0, null);
    var handler__20448 = cljs.core.nth.call(null, vec__20445__20446, 1, null);
    return $elem.off(jayq.core.__GT_event.call(null, events), jayq.core.__GT_selector.call(null, sel__20447), handler__20448)
  };
  var off = function($elem, events, var_args) {
    var p__20440 = null;
    if(goog.isDef(var_args)) {
      p__20440 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return off__delegate.call(this, $elem, events, p__20440)
  };
  off.cljs$lang$maxFixedArity = 2;
  off.cljs$lang$applyTo = function(arglist__20449) {
    var $elem = cljs.core.first(arglist__20449);
    var events = cljs.core.first(cljs.core.next(arglist__20449));
    var p__20440 = cljs.core.rest(cljs.core.next(arglist__20449));
    return off__delegate($elem, events, p__20440)
  };
  off.cljs$lang$arity$variadic = off__delegate;
  return off
}();
jayq.core.prevent = function prevent(e) {
  return e.preventDefault()
};
goog.provide("carneades.policy_analysis.web.test.questions");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("jayq.core");
goog.require("catb.views.pmt.questions");
goog.require("catb.views.pmt.questions");
goog.require("jayq.util");
goog.require("jayq.core");
carneades.policy_analysis.web.test.questions.q_role_yn = cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'category_name", "\ufdd0'hint", "\ufdd0'text", "\ufdd0'role", "\ufdd0'yesnoquestion"], {"\ufdd0'id":"\ufdd1'q-role-yn", "\ufdd0'category_name":"License (Role Y/N)", "\ufdd0'hint":"License information.", "\ufdd0'text":"Does X has a license to do Y?", "\ufdd0'role":true, "\ufdd0'yesnoquestion":true});
carneades.policy_analysis.web.test.questions.q_role = cljs.core.ObjMap.fromObject(["\ufdd0'category_name", "\ufdd0'text", "\ufdd0'typename", "\ufdd0'max", "\ufdd0'type", "\ufdd0'hint", "\ufdd0'min", "\ufdd0'role", "\ufdd0'yesnoquestion", "\ufdd0'id"], {"\ufdd0'category_name":"Usage (Role)", "\ufdd0'text":"Usage X for ?Y", "\ufdd0'typename":cljs.core.PersistentVector.fromArray(["commercial", "non-commercial"], true), "\ufdd0'max":1, "\ufdd0'type":cljs.core.vec(["\ufdd1'commercial", "\ufdd1'non-commercial"]), 
"\ufdd0'hint":"Usage information.", "\ufdd0'min":1, "\ufdd0'role":true, "\ufdd0'yesnoquestion":false, "\ufdd0'id":"\ufdd1'q-role"});
carneades.policy_analysis.web.test.questions.q_concept = cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'category_name", "\ufdd0'hint", "\ufdd0'text", "\ufdd0'concept", "\ufdd0'min", "\ufdd0'max", "\ufdd0'yesnoquestion"], {"\ufdd0'id":"\ufdd1'q-concept", "\ufdd0'category_name":"Father (Concept Y/N)", "\ufdd0'hint":"Father information.", "\ufdd0'text":"Is X the father of Y?", "\ufdd0'concept":true, "\ufdd0'min":1, "\ufdd0'max":1, "\ufdd0'yesnoquestion":true});
carneades.policy_analysis.web.test.questions.q_predicate_yn = cljs.core.ObjMap.fromObject(["\ufdd0'category_name", "\ufdd0'text", "\ufdd0'max", "\ufdd0'hint", "\ufdd0'min", "\ufdd0'yesnoquestion", "\ufdd0'answers", "\ufdd0'id", "\ufdd0'formalanswers"], {"\ufdd0'category_name":"Type of use (Predicate Y/N)", "\ufdd0'text":"a uses b for commercial purposes", "\ufdd0'max":1, "\ufdd0'hint":"Type of use information.", "\ufdd0'min":1, "\ufdd0'yesnoquestion":true, "\ufdd0'answers":cljs.core.PersistentVector.fromArray(["Yes", 
"No", "Maybe"], true), "\ufdd0'id":"\ufdd1'q-predicate-yn", "\ufdd0'formalanswers":cljs.core.PersistentVector.fromArray(["yes", "no", "maybe"], true)});
carneades.policy_analysis.web.test.questions.q_predicate = cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'category_name", "\ufdd0'hint", "\ufdd0'text", "\ufdd0'widgets", "\ufdd0'min", "\ufdd0'max", "\ufdd0'yesnoquestion"], {"\ufdd0'id":"\ufdd1'q-predicate", "\ufdd0'category_name":"Type of use (Predicate)", "\ufdd0'hint":"Type of use information.", "\ufdd0'text":"?a uses ?b for ?p purposes", "\ufdd0'widgets":cljs.core.PersistentVector.fromArray(["text", "text", "text"], true), "\ufdd0'min":1, "\ufdd0'max":1, 
"\ufdd0'yesnoquestion":false});
carneades.policy_analysis.web.test.questions.questions = cljs.core.PersistentVector.fromArray([carneades.policy_analysis.web.test.questions.q_role_yn, carneades.policy_analysis.web.test.questions.q_role, carneades.policy_analysis.web.test.questions.q_concept, carneades.policy_analysis.web.test.questions.q_predicate_yn, carneades.policy_analysis.web.test.questions.q_predicate], true);
carneades.policy_analysis.web.test.questions.prepare = function prepare() {
  var G__6590__6591 = cljs.core.seq.call(null, carneades.policy_analysis.web.test.questions.questions);
  while(true) {
    if(G__6590__6591) {
      var q__6592 = cljs.core.first.call(null, G__6590__6591);
      catb.views.pmt.questions.show_questions.call(null, jayq.util.clj__GT_js.call(null, cljs.core.PersistentVector.fromArray([q__6592], true)), jayq.core.$.call(null, "#questions"), function(G__6590__6591, q__6592) {
        return function(_) {
          return alert("on submit")
        }
      }(G__6590__6591, q__6592));
      var G__6593 = cljs.core.next.call(null, G__6590__6591);
      G__6590__6591 = G__6593;
      continue
    }else {
      return null
    }
    break
  }
};
goog.exportSymbol("carneades.policy_analysis.web.test.questions.prepare", carneades.policy_analysis.web.test.questions.prepare);
carneades.policy_analysis.web.test.questions.run = function run() {
  return true
};
goog.exportSymbol("carneades.policy_analysis.web.test.questions.run", carneades.policy_analysis.web.test.questions.run);
goog.provide("carneades.policy_analysis.web.test");
goog.require("cljs.core");
goog.require("catb.test.navigation");
carneades.policy_analysis.web.test.success = 0;
carneades.policy_analysis.web.test.run = function run() {
  console.log("Example test started.");
  navigation.run.call(null);
  return carneades.policy_analysis.web.test.success
};
goog.exportSymbol("carneades.policy_analysis.web.test.run", carneades.policy_analysis.web.test.run);
