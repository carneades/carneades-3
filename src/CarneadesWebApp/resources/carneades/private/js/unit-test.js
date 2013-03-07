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
goog.provide("goog.debug.Error");
goog.debug.Error = function(opt_msg) {
  this.stack = (new Error).stack || "";
  if(opt_msg) {
    this.message = String(opt_msg)
  }
};
goog.inherits(goog.debug.Error, Error);
goog.debug.Error.prototype.name = "CustomError";
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
  var x__16707 = x == null ? null : x;
  if(p[goog.typeOf(x__16707)]) {
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
    var G__16708__delegate = function(array, i, idxs) {
      return cljs.core.apply.call(null, aget, aget.call(null, array, i), idxs)
    };
    var G__16708 = function(array, i, var_args) {
      var idxs = null;
      if(goog.isDef(var_args)) {
        idxs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__16708__delegate.call(this, array, i, idxs)
    };
    G__16708.cljs$lang$maxFixedArity = 2;
    G__16708.cljs$lang$applyTo = function(arglist__16709) {
      var array = cljs.core.first(arglist__16709);
      var i = cljs.core.first(cljs.core.next(arglist__16709));
      var idxs = cljs.core.rest(cljs.core.next(arglist__16709));
      return G__16708__delegate(array, i, idxs)
    };
    G__16708.cljs$lang$arity$variadic = G__16708__delegate;
    return G__16708
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
      var and__3822__auto____16794 = this$;
      if(and__3822__auto____16794) {
        return this$.cljs$core$IFn$_invoke$arity$1
      }else {
        return and__3822__auto____16794
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$1(this$)
    }else {
      var x__2431__auto____16795 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16796 = cljs.core._invoke[goog.typeOf(x__2431__auto____16795)];
        if(or__3824__auto____16796) {
          return or__3824__auto____16796
        }else {
          var or__3824__auto____16797 = cljs.core._invoke["_"];
          if(or__3824__auto____16797) {
            return or__3824__auto____16797
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$)
    }
  };
  var _invoke__2 = function(this$, a) {
    if(function() {
      var and__3822__auto____16798 = this$;
      if(and__3822__auto____16798) {
        return this$.cljs$core$IFn$_invoke$arity$2
      }else {
        return and__3822__auto____16798
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$2(this$, a)
    }else {
      var x__2431__auto____16799 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16800 = cljs.core._invoke[goog.typeOf(x__2431__auto____16799)];
        if(or__3824__auto____16800) {
          return or__3824__auto____16800
        }else {
          var or__3824__auto____16801 = cljs.core._invoke["_"];
          if(or__3824__auto____16801) {
            return or__3824__auto____16801
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a)
    }
  };
  var _invoke__3 = function(this$, a, b) {
    if(function() {
      var and__3822__auto____16802 = this$;
      if(and__3822__auto____16802) {
        return this$.cljs$core$IFn$_invoke$arity$3
      }else {
        return and__3822__auto____16802
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$3(this$, a, b)
    }else {
      var x__2431__auto____16803 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16804 = cljs.core._invoke[goog.typeOf(x__2431__auto____16803)];
        if(or__3824__auto____16804) {
          return or__3824__auto____16804
        }else {
          var or__3824__auto____16805 = cljs.core._invoke["_"];
          if(or__3824__auto____16805) {
            return or__3824__auto____16805
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b)
    }
  };
  var _invoke__4 = function(this$, a, b, c) {
    if(function() {
      var and__3822__auto____16806 = this$;
      if(and__3822__auto____16806) {
        return this$.cljs$core$IFn$_invoke$arity$4
      }else {
        return and__3822__auto____16806
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$4(this$, a, b, c)
    }else {
      var x__2431__auto____16807 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16808 = cljs.core._invoke[goog.typeOf(x__2431__auto____16807)];
        if(or__3824__auto____16808) {
          return or__3824__auto____16808
        }else {
          var or__3824__auto____16809 = cljs.core._invoke["_"];
          if(or__3824__auto____16809) {
            return or__3824__auto____16809
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c)
    }
  };
  var _invoke__5 = function(this$, a, b, c, d) {
    if(function() {
      var and__3822__auto____16810 = this$;
      if(and__3822__auto____16810) {
        return this$.cljs$core$IFn$_invoke$arity$5
      }else {
        return and__3822__auto____16810
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$5(this$, a, b, c, d)
    }else {
      var x__2431__auto____16811 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16812 = cljs.core._invoke[goog.typeOf(x__2431__auto____16811)];
        if(or__3824__auto____16812) {
          return or__3824__auto____16812
        }else {
          var or__3824__auto____16813 = cljs.core._invoke["_"];
          if(or__3824__auto____16813) {
            return or__3824__auto____16813
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d)
    }
  };
  var _invoke__6 = function(this$, a, b, c, d, e) {
    if(function() {
      var and__3822__auto____16814 = this$;
      if(and__3822__auto____16814) {
        return this$.cljs$core$IFn$_invoke$arity$6
      }else {
        return and__3822__auto____16814
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$6(this$, a, b, c, d, e)
    }else {
      var x__2431__auto____16815 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16816 = cljs.core._invoke[goog.typeOf(x__2431__auto____16815)];
        if(or__3824__auto____16816) {
          return or__3824__auto____16816
        }else {
          var or__3824__auto____16817 = cljs.core._invoke["_"];
          if(or__3824__auto____16817) {
            return or__3824__auto____16817
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e)
    }
  };
  var _invoke__7 = function(this$, a, b, c, d, e, f) {
    if(function() {
      var and__3822__auto____16818 = this$;
      if(and__3822__auto____16818) {
        return this$.cljs$core$IFn$_invoke$arity$7
      }else {
        return and__3822__auto____16818
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$7(this$, a, b, c, d, e, f)
    }else {
      var x__2431__auto____16819 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16820 = cljs.core._invoke[goog.typeOf(x__2431__auto____16819)];
        if(or__3824__auto____16820) {
          return or__3824__auto____16820
        }else {
          var or__3824__auto____16821 = cljs.core._invoke["_"];
          if(or__3824__auto____16821) {
            return or__3824__auto____16821
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f)
    }
  };
  var _invoke__8 = function(this$, a, b, c, d, e, f, g) {
    if(function() {
      var and__3822__auto____16822 = this$;
      if(and__3822__auto____16822) {
        return this$.cljs$core$IFn$_invoke$arity$8
      }else {
        return and__3822__auto____16822
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$8(this$, a, b, c, d, e, f, g)
    }else {
      var x__2431__auto____16823 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16824 = cljs.core._invoke[goog.typeOf(x__2431__auto____16823)];
        if(or__3824__auto____16824) {
          return or__3824__auto____16824
        }else {
          var or__3824__auto____16825 = cljs.core._invoke["_"];
          if(or__3824__auto____16825) {
            return or__3824__auto____16825
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g)
    }
  };
  var _invoke__9 = function(this$, a, b, c, d, e, f, g, h) {
    if(function() {
      var and__3822__auto____16826 = this$;
      if(and__3822__auto____16826) {
        return this$.cljs$core$IFn$_invoke$arity$9
      }else {
        return and__3822__auto____16826
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$9(this$, a, b, c, d, e, f, g, h)
    }else {
      var x__2431__auto____16827 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16828 = cljs.core._invoke[goog.typeOf(x__2431__auto____16827)];
        if(or__3824__auto____16828) {
          return or__3824__auto____16828
        }else {
          var or__3824__auto____16829 = cljs.core._invoke["_"];
          if(or__3824__auto____16829) {
            return or__3824__auto____16829
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h)
    }
  };
  var _invoke__10 = function(this$, a, b, c, d, e, f, g, h, i) {
    if(function() {
      var and__3822__auto____16830 = this$;
      if(and__3822__auto____16830) {
        return this$.cljs$core$IFn$_invoke$arity$10
      }else {
        return and__3822__auto____16830
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$10(this$, a, b, c, d, e, f, g, h, i)
    }else {
      var x__2431__auto____16831 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16832 = cljs.core._invoke[goog.typeOf(x__2431__auto____16831)];
        if(or__3824__auto____16832) {
          return or__3824__auto____16832
        }else {
          var or__3824__auto____16833 = cljs.core._invoke["_"];
          if(or__3824__auto____16833) {
            return or__3824__auto____16833
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i)
    }
  };
  var _invoke__11 = function(this$, a, b, c, d, e, f, g, h, i, j) {
    if(function() {
      var and__3822__auto____16834 = this$;
      if(and__3822__auto____16834) {
        return this$.cljs$core$IFn$_invoke$arity$11
      }else {
        return and__3822__auto____16834
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$11(this$, a, b, c, d, e, f, g, h, i, j)
    }else {
      var x__2431__auto____16835 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16836 = cljs.core._invoke[goog.typeOf(x__2431__auto____16835)];
        if(or__3824__auto____16836) {
          return or__3824__auto____16836
        }else {
          var or__3824__auto____16837 = cljs.core._invoke["_"];
          if(or__3824__auto____16837) {
            return or__3824__auto____16837
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j)
    }
  };
  var _invoke__12 = function(this$, a, b, c, d, e, f, g, h, i, j, k) {
    if(function() {
      var and__3822__auto____16838 = this$;
      if(and__3822__auto____16838) {
        return this$.cljs$core$IFn$_invoke$arity$12
      }else {
        return and__3822__auto____16838
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$12(this$, a, b, c, d, e, f, g, h, i, j, k)
    }else {
      var x__2431__auto____16839 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16840 = cljs.core._invoke[goog.typeOf(x__2431__auto____16839)];
        if(or__3824__auto____16840) {
          return or__3824__auto____16840
        }else {
          var or__3824__auto____16841 = cljs.core._invoke["_"];
          if(or__3824__auto____16841) {
            return or__3824__auto____16841
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k)
    }
  };
  var _invoke__13 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l) {
    if(function() {
      var and__3822__auto____16842 = this$;
      if(and__3822__auto____16842) {
        return this$.cljs$core$IFn$_invoke$arity$13
      }else {
        return and__3822__auto____16842
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$13(this$, a, b, c, d, e, f, g, h, i, j, k, l)
    }else {
      var x__2431__auto____16843 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16844 = cljs.core._invoke[goog.typeOf(x__2431__auto____16843)];
        if(or__3824__auto____16844) {
          return or__3824__auto____16844
        }else {
          var or__3824__auto____16845 = cljs.core._invoke["_"];
          if(or__3824__auto____16845) {
            return or__3824__auto____16845
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l)
    }
  };
  var _invoke__14 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m) {
    if(function() {
      var and__3822__auto____16846 = this$;
      if(and__3822__auto____16846) {
        return this$.cljs$core$IFn$_invoke$arity$14
      }else {
        return and__3822__auto____16846
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$14(this$, a, b, c, d, e, f, g, h, i, j, k, l, m)
    }else {
      var x__2431__auto____16847 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16848 = cljs.core._invoke[goog.typeOf(x__2431__auto____16847)];
        if(or__3824__auto____16848) {
          return or__3824__auto____16848
        }else {
          var or__3824__auto____16849 = cljs.core._invoke["_"];
          if(or__3824__auto____16849) {
            return or__3824__auto____16849
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m)
    }
  };
  var _invoke__15 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n) {
    if(function() {
      var and__3822__auto____16850 = this$;
      if(and__3822__auto____16850) {
        return this$.cljs$core$IFn$_invoke$arity$15
      }else {
        return and__3822__auto____16850
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$15(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    }else {
      var x__2431__auto____16851 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16852 = cljs.core._invoke[goog.typeOf(x__2431__auto____16851)];
        if(or__3824__auto____16852) {
          return or__3824__auto____16852
        }else {
          var or__3824__auto____16853 = cljs.core._invoke["_"];
          if(or__3824__auto____16853) {
            return or__3824__auto____16853
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    }
  };
  var _invoke__16 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) {
    if(function() {
      var and__3822__auto____16854 = this$;
      if(and__3822__auto____16854) {
        return this$.cljs$core$IFn$_invoke$arity$16
      }else {
        return and__3822__auto____16854
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$16(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }else {
      var x__2431__auto____16855 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16856 = cljs.core._invoke[goog.typeOf(x__2431__auto____16855)];
        if(or__3824__auto____16856) {
          return or__3824__auto____16856
        }else {
          var or__3824__auto____16857 = cljs.core._invoke["_"];
          if(or__3824__auto____16857) {
            return or__3824__auto____16857
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    }
  };
  var _invoke__17 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) {
    if(function() {
      var and__3822__auto____16858 = this$;
      if(and__3822__auto____16858) {
        return this$.cljs$core$IFn$_invoke$arity$17
      }else {
        return and__3822__auto____16858
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$17(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    }else {
      var x__2431__auto____16859 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16860 = cljs.core._invoke[goog.typeOf(x__2431__auto____16859)];
        if(or__3824__auto____16860) {
          return or__3824__auto____16860
        }else {
          var or__3824__auto____16861 = cljs.core._invoke["_"];
          if(or__3824__auto____16861) {
            return or__3824__auto____16861
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    }
  };
  var _invoke__18 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) {
    if(function() {
      var and__3822__auto____16862 = this$;
      if(and__3822__auto____16862) {
        return this$.cljs$core$IFn$_invoke$arity$18
      }else {
        return and__3822__auto____16862
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$18(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }else {
      var x__2431__auto____16863 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16864 = cljs.core._invoke[goog.typeOf(x__2431__auto____16863)];
        if(or__3824__auto____16864) {
          return or__3824__auto____16864
        }else {
          var or__3824__auto____16865 = cljs.core._invoke["_"];
          if(or__3824__auto____16865) {
            return or__3824__auto____16865
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    }
  };
  var _invoke__19 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s) {
    if(function() {
      var and__3822__auto____16866 = this$;
      if(and__3822__auto____16866) {
        return this$.cljs$core$IFn$_invoke$arity$19
      }else {
        return and__3822__auto____16866
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$19(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s)
    }else {
      var x__2431__auto____16867 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16868 = cljs.core._invoke[goog.typeOf(x__2431__auto____16867)];
        if(or__3824__auto____16868) {
          return or__3824__auto____16868
        }else {
          var or__3824__auto____16869 = cljs.core._invoke["_"];
          if(or__3824__auto____16869) {
            return or__3824__auto____16869
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s)
    }
  };
  var _invoke__20 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t) {
    if(function() {
      var and__3822__auto____16870 = this$;
      if(and__3822__auto____16870) {
        return this$.cljs$core$IFn$_invoke$arity$20
      }else {
        return and__3822__auto____16870
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$20(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t)
    }else {
      var x__2431__auto____16871 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16872 = cljs.core._invoke[goog.typeOf(x__2431__auto____16871)];
        if(or__3824__auto____16872) {
          return or__3824__auto____16872
        }else {
          var or__3824__auto____16873 = cljs.core._invoke["_"];
          if(or__3824__auto____16873) {
            return or__3824__auto____16873
          }else {
            throw cljs.core.missing_protocol.call(null, "IFn.-invoke", this$);
          }
        }
      }().call(null, this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t)
    }
  };
  var _invoke__21 = function(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest) {
    if(function() {
      var and__3822__auto____16874 = this$;
      if(and__3822__auto____16874) {
        return this$.cljs$core$IFn$_invoke$arity$21
      }else {
        return and__3822__auto____16874
      }
    }()) {
      return this$.cljs$core$IFn$_invoke$arity$21(this$, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s, t, rest)
    }else {
      var x__2431__auto____16875 = this$ == null ? null : this$;
      return function() {
        var or__3824__auto____16876 = cljs.core._invoke[goog.typeOf(x__2431__auto____16875)];
        if(or__3824__auto____16876) {
          return or__3824__auto____16876
        }else {
          var or__3824__auto____16877 = cljs.core._invoke["_"];
          if(or__3824__auto____16877) {
            return or__3824__auto____16877
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
    var and__3822__auto____16882 = coll;
    if(and__3822__auto____16882) {
      return coll.cljs$core$ICounted$_count$arity$1
    }else {
      return and__3822__auto____16882
    }
  }()) {
    return coll.cljs$core$ICounted$_count$arity$1(coll)
  }else {
    var x__2431__auto____16883 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16884 = cljs.core._count[goog.typeOf(x__2431__auto____16883)];
      if(or__3824__auto____16884) {
        return or__3824__auto____16884
      }else {
        var or__3824__auto____16885 = cljs.core._count["_"];
        if(or__3824__auto____16885) {
          return or__3824__auto____16885
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
    var and__3822__auto____16890 = coll;
    if(and__3822__auto____16890) {
      return coll.cljs$core$IEmptyableCollection$_empty$arity$1
    }else {
      return and__3822__auto____16890
    }
  }()) {
    return coll.cljs$core$IEmptyableCollection$_empty$arity$1(coll)
  }else {
    var x__2431__auto____16891 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16892 = cljs.core._empty[goog.typeOf(x__2431__auto____16891)];
      if(or__3824__auto____16892) {
        return or__3824__auto____16892
      }else {
        var or__3824__auto____16893 = cljs.core._empty["_"];
        if(or__3824__auto____16893) {
          return or__3824__auto____16893
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
    var and__3822__auto____16898 = coll;
    if(and__3822__auto____16898) {
      return coll.cljs$core$ICollection$_conj$arity$2
    }else {
      return and__3822__auto____16898
    }
  }()) {
    return coll.cljs$core$ICollection$_conj$arity$2(coll, o)
  }else {
    var x__2431__auto____16899 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16900 = cljs.core._conj[goog.typeOf(x__2431__auto____16899)];
      if(or__3824__auto____16900) {
        return or__3824__auto____16900
      }else {
        var or__3824__auto____16901 = cljs.core._conj["_"];
        if(or__3824__auto____16901) {
          return or__3824__auto____16901
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
      var and__3822__auto____16910 = coll;
      if(and__3822__auto____16910) {
        return coll.cljs$core$IIndexed$_nth$arity$2
      }else {
        return and__3822__auto____16910
      }
    }()) {
      return coll.cljs$core$IIndexed$_nth$arity$2(coll, n)
    }else {
      var x__2431__auto____16911 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____16912 = cljs.core._nth[goog.typeOf(x__2431__auto____16911)];
        if(or__3824__auto____16912) {
          return or__3824__auto____16912
        }else {
          var or__3824__auto____16913 = cljs.core._nth["_"];
          if(or__3824__auto____16913) {
            return or__3824__auto____16913
          }else {
            throw cljs.core.missing_protocol.call(null, "IIndexed.-nth", coll);
          }
        }
      }().call(null, coll, n)
    }
  };
  var _nth__3 = function(coll, n, not_found) {
    if(function() {
      var and__3822__auto____16914 = coll;
      if(and__3822__auto____16914) {
        return coll.cljs$core$IIndexed$_nth$arity$3
      }else {
        return and__3822__auto____16914
      }
    }()) {
      return coll.cljs$core$IIndexed$_nth$arity$3(coll, n, not_found)
    }else {
      var x__2431__auto____16915 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____16916 = cljs.core._nth[goog.typeOf(x__2431__auto____16915)];
        if(or__3824__auto____16916) {
          return or__3824__auto____16916
        }else {
          var or__3824__auto____16917 = cljs.core._nth["_"];
          if(or__3824__auto____16917) {
            return or__3824__auto____16917
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
    var and__3822__auto____16922 = coll;
    if(and__3822__auto____16922) {
      return coll.cljs$core$ISeq$_first$arity$1
    }else {
      return and__3822__auto____16922
    }
  }()) {
    return coll.cljs$core$ISeq$_first$arity$1(coll)
  }else {
    var x__2431__auto____16923 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16924 = cljs.core._first[goog.typeOf(x__2431__auto____16923)];
      if(or__3824__auto____16924) {
        return or__3824__auto____16924
      }else {
        var or__3824__auto____16925 = cljs.core._first["_"];
        if(or__3824__auto____16925) {
          return or__3824__auto____16925
        }else {
          throw cljs.core.missing_protocol.call(null, "ISeq.-first", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._rest = function _rest(coll) {
  if(function() {
    var and__3822__auto____16930 = coll;
    if(and__3822__auto____16930) {
      return coll.cljs$core$ISeq$_rest$arity$1
    }else {
      return and__3822__auto____16930
    }
  }()) {
    return coll.cljs$core$ISeq$_rest$arity$1(coll)
  }else {
    var x__2431__auto____16931 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16932 = cljs.core._rest[goog.typeOf(x__2431__auto____16931)];
      if(or__3824__auto____16932) {
        return or__3824__auto____16932
      }else {
        var or__3824__auto____16933 = cljs.core._rest["_"];
        if(or__3824__auto____16933) {
          return or__3824__auto____16933
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
    var and__3822__auto____16938 = coll;
    if(and__3822__auto____16938) {
      return coll.cljs$core$INext$_next$arity$1
    }else {
      return and__3822__auto____16938
    }
  }()) {
    return coll.cljs$core$INext$_next$arity$1(coll)
  }else {
    var x__2431__auto____16939 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16940 = cljs.core._next[goog.typeOf(x__2431__auto____16939)];
      if(or__3824__auto____16940) {
        return or__3824__auto____16940
      }else {
        var or__3824__auto____16941 = cljs.core._next["_"];
        if(or__3824__auto____16941) {
          return or__3824__auto____16941
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
      var and__3822__auto____16950 = o;
      if(and__3822__auto____16950) {
        return o.cljs$core$ILookup$_lookup$arity$2
      }else {
        return and__3822__auto____16950
      }
    }()) {
      return o.cljs$core$ILookup$_lookup$arity$2(o, k)
    }else {
      var x__2431__auto____16951 = o == null ? null : o;
      return function() {
        var or__3824__auto____16952 = cljs.core._lookup[goog.typeOf(x__2431__auto____16951)];
        if(or__3824__auto____16952) {
          return or__3824__auto____16952
        }else {
          var or__3824__auto____16953 = cljs.core._lookup["_"];
          if(or__3824__auto____16953) {
            return or__3824__auto____16953
          }else {
            throw cljs.core.missing_protocol.call(null, "ILookup.-lookup", o);
          }
        }
      }().call(null, o, k)
    }
  };
  var _lookup__3 = function(o, k, not_found) {
    if(function() {
      var and__3822__auto____16954 = o;
      if(and__3822__auto____16954) {
        return o.cljs$core$ILookup$_lookup$arity$3
      }else {
        return and__3822__auto____16954
      }
    }()) {
      return o.cljs$core$ILookup$_lookup$arity$3(o, k, not_found)
    }else {
      var x__2431__auto____16955 = o == null ? null : o;
      return function() {
        var or__3824__auto____16956 = cljs.core._lookup[goog.typeOf(x__2431__auto____16955)];
        if(or__3824__auto____16956) {
          return or__3824__auto____16956
        }else {
          var or__3824__auto____16957 = cljs.core._lookup["_"];
          if(or__3824__auto____16957) {
            return or__3824__auto____16957
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
    var and__3822__auto____16962 = coll;
    if(and__3822__auto____16962) {
      return coll.cljs$core$IAssociative$_contains_key_QMARK_$arity$2
    }else {
      return and__3822__auto____16962
    }
  }()) {
    return coll.cljs$core$IAssociative$_contains_key_QMARK_$arity$2(coll, k)
  }else {
    var x__2431__auto____16963 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16964 = cljs.core._contains_key_QMARK_[goog.typeOf(x__2431__auto____16963)];
      if(or__3824__auto____16964) {
        return or__3824__auto____16964
      }else {
        var or__3824__auto____16965 = cljs.core._contains_key_QMARK_["_"];
        if(or__3824__auto____16965) {
          return or__3824__auto____16965
        }else {
          throw cljs.core.missing_protocol.call(null, "IAssociative.-contains-key?", coll);
        }
      }
    }().call(null, coll, k)
  }
};
cljs.core._assoc = function _assoc(coll, k, v) {
  if(function() {
    var and__3822__auto____16970 = coll;
    if(and__3822__auto____16970) {
      return coll.cljs$core$IAssociative$_assoc$arity$3
    }else {
      return and__3822__auto____16970
    }
  }()) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, k, v)
  }else {
    var x__2431__auto____16971 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16972 = cljs.core._assoc[goog.typeOf(x__2431__auto____16971)];
      if(or__3824__auto____16972) {
        return or__3824__auto____16972
      }else {
        var or__3824__auto____16973 = cljs.core._assoc["_"];
        if(or__3824__auto____16973) {
          return or__3824__auto____16973
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
    var and__3822__auto____16978 = coll;
    if(and__3822__auto____16978) {
      return coll.cljs$core$IMap$_dissoc$arity$2
    }else {
      return and__3822__auto____16978
    }
  }()) {
    return coll.cljs$core$IMap$_dissoc$arity$2(coll, k)
  }else {
    var x__2431__auto____16979 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16980 = cljs.core._dissoc[goog.typeOf(x__2431__auto____16979)];
      if(or__3824__auto____16980) {
        return or__3824__auto____16980
      }else {
        var or__3824__auto____16981 = cljs.core._dissoc["_"];
        if(or__3824__auto____16981) {
          return or__3824__auto____16981
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
    var and__3822__auto____16986 = coll;
    if(and__3822__auto____16986) {
      return coll.cljs$core$IMapEntry$_key$arity$1
    }else {
      return and__3822__auto____16986
    }
  }()) {
    return coll.cljs$core$IMapEntry$_key$arity$1(coll)
  }else {
    var x__2431__auto____16987 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16988 = cljs.core._key[goog.typeOf(x__2431__auto____16987)];
      if(or__3824__auto____16988) {
        return or__3824__auto____16988
      }else {
        var or__3824__auto____16989 = cljs.core._key["_"];
        if(or__3824__auto____16989) {
          return or__3824__auto____16989
        }else {
          throw cljs.core.missing_protocol.call(null, "IMapEntry.-key", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._val = function _val(coll) {
  if(function() {
    var and__3822__auto____16994 = coll;
    if(and__3822__auto____16994) {
      return coll.cljs$core$IMapEntry$_val$arity$1
    }else {
      return and__3822__auto____16994
    }
  }()) {
    return coll.cljs$core$IMapEntry$_val$arity$1(coll)
  }else {
    var x__2431__auto____16995 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____16996 = cljs.core._val[goog.typeOf(x__2431__auto____16995)];
      if(or__3824__auto____16996) {
        return or__3824__auto____16996
      }else {
        var or__3824__auto____16997 = cljs.core._val["_"];
        if(or__3824__auto____16997) {
          return or__3824__auto____16997
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
    var and__3822__auto____17002 = coll;
    if(and__3822__auto____17002) {
      return coll.cljs$core$ISet$_disjoin$arity$2
    }else {
      return and__3822__auto____17002
    }
  }()) {
    return coll.cljs$core$ISet$_disjoin$arity$2(coll, v)
  }else {
    var x__2431__auto____17003 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17004 = cljs.core._disjoin[goog.typeOf(x__2431__auto____17003)];
      if(or__3824__auto____17004) {
        return or__3824__auto____17004
      }else {
        var or__3824__auto____17005 = cljs.core._disjoin["_"];
        if(or__3824__auto____17005) {
          return or__3824__auto____17005
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
    var and__3822__auto____17010 = coll;
    if(and__3822__auto____17010) {
      return coll.cljs$core$IStack$_peek$arity$1
    }else {
      return and__3822__auto____17010
    }
  }()) {
    return coll.cljs$core$IStack$_peek$arity$1(coll)
  }else {
    var x__2431__auto____17011 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17012 = cljs.core._peek[goog.typeOf(x__2431__auto____17011)];
      if(or__3824__auto____17012) {
        return or__3824__auto____17012
      }else {
        var or__3824__auto____17013 = cljs.core._peek["_"];
        if(or__3824__auto____17013) {
          return or__3824__auto____17013
        }else {
          throw cljs.core.missing_protocol.call(null, "IStack.-peek", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._pop = function _pop(coll) {
  if(function() {
    var and__3822__auto____17018 = coll;
    if(and__3822__auto____17018) {
      return coll.cljs$core$IStack$_pop$arity$1
    }else {
      return and__3822__auto____17018
    }
  }()) {
    return coll.cljs$core$IStack$_pop$arity$1(coll)
  }else {
    var x__2431__auto____17019 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17020 = cljs.core._pop[goog.typeOf(x__2431__auto____17019)];
      if(or__3824__auto____17020) {
        return or__3824__auto____17020
      }else {
        var or__3824__auto____17021 = cljs.core._pop["_"];
        if(or__3824__auto____17021) {
          return or__3824__auto____17021
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
    var and__3822__auto____17026 = coll;
    if(and__3822__auto____17026) {
      return coll.cljs$core$IVector$_assoc_n$arity$3
    }else {
      return and__3822__auto____17026
    }
  }()) {
    return coll.cljs$core$IVector$_assoc_n$arity$3(coll, n, val)
  }else {
    var x__2431__auto____17027 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17028 = cljs.core._assoc_n[goog.typeOf(x__2431__auto____17027)];
      if(or__3824__auto____17028) {
        return or__3824__auto____17028
      }else {
        var or__3824__auto____17029 = cljs.core._assoc_n["_"];
        if(or__3824__auto____17029) {
          return or__3824__auto____17029
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
    var and__3822__auto____17034 = o;
    if(and__3822__auto____17034) {
      return o.cljs$core$IDeref$_deref$arity$1
    }else {
      return and__3822__auto____17034
    }
  }()) {
    return o.cljs$core$IDeref$_deref$arity$1(o)
  }else {
    var x__2431__auto____17035 = o == null ? null : o;
    return function() {
      var or__3824__auto____17036 = cljs.core._deref[goog.typeOf(x__2431__auto____17035)];
      if(or__3824__auto____17036) {
        return or__3824__auto____17036
      }else {
        var or__3824__auto____17037 = cljs.core._deref["_"];
        if(or__3824__auto____17037) {
          return or__3824__auto____17037
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
    var and__3822__auto____17042 = o;
    if(and__3822__auto____17042) {
      return o.cljs$core$IDerefWithTimeout$_deref_with_timeout$arity$3
    }else {
      return and__3822__auto____17042
    }
  }()) {
    return o.cljs$core$IDerefWithTimeout$_deref_with_timeout$arity$3(o, msec, timeout_val)
  }else {
    var x__2431__auto____17043 = o == null ? null : o;
    return function() {
      var or__3824__auto____17044 = cljs.core._deref_with_timeout[goog.typeOf(x__2431__auto____17043)];
      if(or__3824__auto____17044) {
        return or__3824__auto____17044
      }else {
        var or__3824__auto____17045 = cljs.core._deref_with_timeout["_"];
        if(or__3824__auto____17045) {
          return or__3824__auto____17045
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
    var and__3822__auto____17050 = o;
    if(and__3822__auto____17050) {
      return o.cljs$core$IMeta$_meta$arity$1
    }else {
      return and__3822__auto____17050
    }
  }()) {
    return o.cljs$core$IMeta$_meta$arity$1(o)
  }else {
    var x__2431__auto____17051 = o == null ? null : o;
    return function() {
      var or__3824__auto____17052 = cljs.core._meta[goog.typeOf(x__2431__auto____17051)];
      if(or__3824__auto____17052) {
        return or__3824__auto____17052
      }else {
        var or__3824__auto____17053 = cljs.core._meta["_"];
        if(or__3824__auto____17053) {
          return or__3824__auto____17053
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
    var and__3822__auto____17058 = o;
    if(and__3822__auto____17058) {
      return o.cljs$core$IWithMeta$_with_meta$arity$2
    }else {
      return and__3822__auto____17058
    }
  }()) {
    return o.cljs$core$IWithMeta$_with_meta$arity$2(o, meta)
  }else {
    var x__2431__auto____17059 = o == null ? null : o;
    return function() {
      var or__3824__auto____17060 = cljs.core._with_meta[goog.typeOf(x__2431__auto____17059)];
      if(or__3824__auto____17060) {
        return or__3824__auto____17060
      }else {
        var or__3824__auto____17061 = cljs.core._with_meta["_"];
        if(or__3824__auto____17061) {
          return or__3824__auto____17061
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
      var and__3822__auto____17070 = coll;
      if(and__3822__auto____17070) {
        return coll.cljs$core$IReduce$_reduce$arity$2
      }else {
        return and__3822__auto____17070
      }
    }()) {
      return coll.cljs$core$IReduce$_reduce$arity$2(coll, f)
    }else {
      var x__2431__auto____17071 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____17072 = cljs.core._reduce[goog.typeOf(x__2431__auto____17071)];
        if(or__3824__auto____17072) {
          return or__3824__auto____17072
        }else {
          var or__3824__auto____17073 = cljs.core._reduce["_"];
          if(or__3824__auto____17073) {
            return or__3824__auto____17073
          }else {
            throw cljs.core.missing_protocol.call(null, "IReduce.-reduce", coll);
          }
        }
      }().call(null, coll, f)
    }
  };
  var _reduce__3 = function(coll, f, start) {
    if(function() {
      var and__3822__auto____17074 = coll;
      if(and__3822__auto____17074) {
        return coll.cljs$core$IReduce$_reduce$arity$3
      }else {
        return and__3822__auto____17074
      }
    }()) {
      return coll.cljs$core$IReduce$_reduce$arity$3(coll, f, start)
    }else {
      var x__2431__auto____17075 = coll == null ? null : coll;
      return function() {
        var or__3824__auto____17076 = cljs.core._reduce[goog.typeOf(x__2431__auto____17075)];
        if(or__3824__auto____17076) {
          return or__3824__auto____17076
        }else {
          var or__3824__auto____17077 = cljs.core._reduce["_"];
          if(or__3824__auto____17077) {
            return or__3824__auto____17077
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
    var and__3822__auto____17082 = coll;
    if(and__3822__auto____17082) {
      return coll.cljs$core$IKVReduce$_kv_reduce$arity$3
    }else {
      return and__3822__auto____17082
    }
  }()) {
    return coll.cljs$core$IKVReduce$_kv_reduce$arity$3(coll, f, init)
  }else {
    var x__2431__auto____17083 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17084 = cljs.core._kv_reduce[goog.typeOf(x__2431__auto____17083)];
      if(or__3824__auto____17084) {
        return or__3824__auto____17084
      }else {
        var or__3824__auto____17085 = cljs.core._kv_reduce["_"];
        if(or__3824__auto____17085) {
          return or__3824__auto____17085
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
    var and__3822__auto____17090 = o;
    if(and__3822__auto____17090) {
      return o.cljs$core$IEquiv$_equiv$arity$2
    }else {
      return and__3822__auto____17090
    }
  }()) {
    return o.cljs$core$IEquiv$_equiv$arity$2(o, other)
  }else {
    var x__2431__auto____17091 = o == null ? null : o;
    return function() {
      var or__3824__auto____17092 = cljs.core._equiv[goog.typeOf(x__2431__auto____17091)];
      if(or__3824__auto____17092) {
        return or__3824__auto____17092
      }else {
        var or__3824__auto____17093 = cljs.core._equiv["_"];
        if(or__3824__auto____17093) {
          return or__3824__auto____17093
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
    var and__3822__auto____17098 = o;
    if(and__3822__auto____17098) {
      return o.cljs$core$IHash$_hash$arity$1
    }else {
      return and__3822__auto____17098
    }
  }()) {
    return o.cljs$core$IHash$_hash$arity$1(o)
  }else {
    var x__2431__auto____17099 = o == null ? null : o;
    return function() {
      var or__3824__auto____17100 = cljs.core._hash[goog.typeOf(x__2431__auto____17099)];
      if(or__3824__auto____17100) {
        return or__3824__auto____17100
      }else {
        var or__3824__auto____17101 = cljs.core._hash["_"];
        if(or__3824__auto____17101) {
          return or__3824__auto____17101
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
    var and__3822__auto____17106 = o;
    if(and__3822__auto____17106) {
      return o.cljs$core$ISeqable$_seq$arity$1
    }else {
      return and__3822__auto____17106
    }
  }()) {
    return o.cljs$core$ISeqable$_seq$arity$1(o)
  }else {
    var x__2431__auto____17107 = o == null ? null : o;
    return function() {
      var or__3824__auto____17108 = cljs.core._seq[goog.typeOf(x__2431__auto____17107)];
      if(or__3824__auto____17108) {
        return or__3824__auto____17108
      }else {
        var or__3824__auto____17109 = cljs.core._seq["_"];
        if(or__3824__auto____17109) {
          return or__3824__auto____17109
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
    var and__3822__auto____17114 = coll;
    if(and__3822__auto____17114) {
      return coll.cljs$core$IReversible$_rseq$arity$1
    }else {
      return and__3822__auto____17114
    }
  }()) {
    return coll.cljs$core$IReversible$_rseq$arity$1(coll)
  }else {
    var x__2431__auto____17115 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17116 = cljs.core._rseq[goog.typeOf(x__2431__auto____17115)];
      if(or__3824__auto____17116) {
        return or__3824__auto____17116
      }else {
        var or__3824__auto____17117 = cljs.core._rseq["_"];
        if(or__3824__auto____17117) {
          return or__3824__auto____17117
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
    var and__3822__auto____17122 = coll;
    if(and__3822__auto____17122) {
      return coll.cljs$core$ISorted$_sorted_seq$arity$2
    }else {
      return and__3822__auto____17122
    }
  }()) {
    return coll.cljs$core$ISorted$_sorted_seq$arity$2(coll, ascending_QMARK_)
  }else {
    var x__2431__auto____17123 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17124 = cljs.core._sorted_seq[goog.typeOf(x__2431__auto____17123)];
      if(or__3824__auto____17124) {
        return or__3824__auto____17124
      }else {
        var or__3824__auto____17125 = cljs.core._sorted_seq["_"];
        if(or__3824__auto____17125) {
          return or__3824__auto____17125
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-sorted-seq", coll);
        }
      }
    }().call(null, coll, ascending_QMARK_)
  }
};
cljs.core._sorted_seq_from = function _sorted_seq_from(coll, k, ascending_QMARK_) {
  if(function() {
    var and__3822__auto____17130 = coll;
    if(and__3822__auto____17130) {
      return coll.cljs$core$ISorted$_sorted_seq_from$arity$3
    }else {
      return and__3822__auto____17130
    }
  }()) {
    return coll.cljs$core$ISorted$_sorted_seq_from$arity$3(coll, k, ascending_QMARK_)
  }else {
    var x__2431__auto____17131 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17132 = cljs.core._sorted_seq_from[goog.typeOf(x__2431__auto____17131)];
      if(or__3824__auto____17132) {
        return or__3824__auto____17132
      }else {
        var or__3824__auto____17133 = cljs.core._sorted_seq_from["_"];
        if(or__3824__auto____17133) {
          return or__3824__auto____17133
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-sorted-seq-from", coll);
        }
      }
    }().call(null, coll, k, ascending_QMARK_)
  }
};
cljs.core._entry_key = function _entry_key(coll, entry) {
  if(function() {
    var and__3822__auto____17138 = coll;
    if(and__3822__auto____17138) {
      return coll.cljs$core$ISorted$_entry_key$arity$2
    }else {
      return and__3822__auto____17138
    }
  }()) {
    return coll.cljs$core$ISorted$_entry_key$arity$2(coll, entry)
  }else {
    var x__2431__auto____17139 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17140 = cljs.core._entry_key[goog.typeOf(x__2431__auto____17139)];
      if(or__3824__auto____17140) {
        return or__3824__auto____17140
      }else {
        var or__3824__auto____17141 = cljs.core._entry_key["_"];
        if(or__3824__auto____17141) {
          return or__3824__auto____17141
        }else {
          throw cljs.core.missing_protocol.call(null, "ISorted.-entry-key", coll);
        }
      }
    }().call(null, coll, entry)
  }
};
cljs.core._comparator = function _comparator(coll) {
  if(function() {
    var and__3822__auto____17146 = coll;
    if(and__3822__auto____17146) {
      return coll.cljs$core$ISorted$_comparator$arity$1
    }else {
      return and__3822__auto____17146
    }
  }()) {
    return coll.cljs$core$ISorted$_comparator$arity$1(coll)
  }else {
    var x__2431__auto____17147 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17148 = cljs.core._comparator[goog.typeOf(x__2431__auto____17147)];
      if(or__3824__auto____17148) {
        return or__3824__auto____17148
      }else {
        var or__3824__auto____17149 = cljs.core._comparator["_"];
        if(or__3824__auto____17149) {
          return or__3824__auto____17149
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
    var and__3822__auto____17154 = o;
    if(and__3822__auto____17154) {
      return o.cljs$core$IPrintable$_pr_seq$arity$2
    }else {
      return and__3822__auto____17154
    }
  }()) {
    return o.cljs$core$IPrintable$_pr_seq$arity$2(o, opts)
  }else {
    var x__2431__auto____17155 = o == null ? null : o;
    return function() {
      var or__3824__auto____17156 = cljs.core._pr_seq[goog.typeOf(x__2431__auto____17155)];
      if(or__3824__auto____17156) {
        return or__3824__auto____17156
      }else {
        var or__3824__auto____17157 = cljs.core._pr_seq["_"];
        if(or__3824__auto____17157) {
          return or__3824__auto____17157
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
    var and__3822__auto____17162 = writer;
    if(and__3822__auto____17162) {
      return writer.cljs$core$IWriter$_write$arity$2
    }else {
      return and__3822__auto____17162
    }
  }()) {
    return writer.cljs$core$IWriter$_write$arity$2(writer, s)
  }else {
    var x__2431__auto____17163 = writer == null ? null : writer;
    return function() {
      var or__3824__auto____17164 = cljs.core._write[goog.typeOf(x__2431__auto____17163)];
      if(or__3824__auto____17164) {
        return or__3824__auto____17164
      }else {
        var or__3824__auto____17165 = cljs.core._write["_"];
        if(or__3824__auto____17165) {
          return or__3824__auto____17165
        }else {
          throw cljs.core.missing_protocol.call(null, "IWriter.-write", writer);
        }
      }
    }().call(null, writer, s)
  }
};
cljs.core._flush = function _flush(writer) {
  if(function() {
    var and__3822__auto____17170 = writer;
    if(and__3822__auto____17170) {
      return writer.cljs$core$IWriter$_flush$arity$1
    }else {
      return and__3822__auto____17170
    }
  }()) {
    return writer.cljs$core$IWriter$_flush$arity$1(writer)
  }else {
    var x__2431__auto____17171 = writer == null ? null : writer;
    return function() {
      var or__3824__auto____17172 = cljs.core._flush[goog.typeOf(x__2431__auto____17171)];
      if(or__3824__auto____17172) {
        return or__3824__auto____17172
      }else {
        var or__3824__auto____17173 = cljs.core._flush["_"];
        if(or__3824__auto____17173) {
          return or__3824__auto____17173
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
    var and__3822__auto____17178 = o;
    if(and__3822__auto____17178) {
      return o.cljs$core$IPrintWithWriter$_pr_writer$arity$3
    }else {
      return and__3822__auto____17178
    }
  }()) {
    return o.cljs$core$IPrintWithWriter$_pr_writer$arity$3(o, writer, opts)
  }else {
    var x__2431__auto____17179 = o == null ? null : o;
    return function() {
      var or__3824__auto____17180 = cljs.core._pr_writer[goog.typeOf(x__2431__auto____17179)];
      if(or__3824__auto____17180) {
        return or__3824__auto____17180
      }else {
        var or__3824__auto____17181 = cljs.core._pr_writer["_"];
        if(or__3824__auto____17181) {
          return or__3824__auto____17181
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
    var and__3822__auto____17186 = d;
    if(and__3822__auto____17186) {
      return d.cljs$core$IPending$_realized_QMARK_$arity$1
    }else {
      return and__3822__auto____17186
    }
  }()) {
    return d.cljs$core$IPending$_realized_QMARK_$arity$1(d)
  }else {
    var x__2431__auto____17187 = d == null ? null : d;
    return function() {
      var or__3824__auto____17188 = cljs.core._realized_QMARK_[goog.typeOf(x__2431__auto____17187)];
      if(or__3824__auto____17188) {
        return or__3824__auto____17188
      }else {
        var or__3824__auto____17189 = cljs.core._realized_QMARK_["_"];
        if(or__3824__auto____17189) {
          return or__3824__auto____17189
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
    var and__3822__auto____17194 = this$;
    if(and__3822__auto____17194) {
      return this$.cljs$core$IWatchable$_notify_watches$arity$3
    }else {
      return and__3822__auto____17194
    }
  }()) {
    return this$.cljs$core$IWatchable$_notify_watches$arity$3(this$, oldval, newval)
  }else {
    var x__2431__auto____17195 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____17196 = cljs.core._notify_watches[goog.typeOf(x__2431__auto____17195)];
      if(or__3824__auto____17196) {
        return or__3824__auto____17196
      }else {
        var or__3824__auto____17197 = cljs.core._notify_watches["_"];
        if(or__3824__auto____17197) {
          return or__3824__auto____17197
        }else {
          throw cljs.core.missing_protocol.call(null, "IWatchable.-notify-watches", this$);
        }
      }
    }().call(null, this$, oldval, newval)
  }
};
cljs.core._add_watch = function _add_watch(this$, key, f) {
  if(function() {
    var and__3822__auto____17202 = this$;
    if(and__3822__auto____17202) {
      return this$.cljs$core$IWatchable$_add_watch$arity$3
    }else {
      return and__3822__auto____17202
    }
  }()) {
    return this$.cljs$core$IWatchable$_add_watch$arity$3(this$, key, f)
  }else {
    var x__2431__auto____17203 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____17204 = cljs.core._add_watch[goog.typeOf(x__2431__auto____17203)];
      if(or__3824__auto____17204) {
        return or__3824__auto____17204
      }else {
        var or__3824__auto____17205 = cljs.core._add_watch["_"];
        if(or__3824__auto____17205) {
          return or__3824__auto____17205
        }else {
          throw cljs.core.missing_protocol.call(null, "IWatchable.-add-watch", this$);
        }
      }
    }().call(null, this$, key, f)
  }
};
cljs.core._remove_watch = function _remove_watch(this$, key) {
  if(function() {
    var and__3822__auto____17210 = this$;
    if(and__3822__auto____17210) {
      return this$.cljs$core$IWatchable$_remove_watch$arity$2
    }else {
      return and__3822__auto____17210
    }
  }()) {
    return this$.cljs$core$IWatchable$_remove_watch$arity$2(this$, key)
  }else {
    var x__2431__auto____17211 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____17212 = cljs.core._remove_watch[goog.typeOf(x__2431__auto____17211)];
      if(or__3824__auto____17212) {
        return or__3824__auto____17212
      }else {
        var or__3824__auto____17213 = cljs.core._remove_watch["_"];
        if(or__3824__auto____17213) {
          return or__3824__auto____17213
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
    var and__3822__auto____17218 = coll;
    if(and__3822__auto____17218) {
      return coll.cljs$core$IEditableCollection$_as_transient$arity$1
    }else {
      return and__3822__auto____17218
    }
  }()) {
    return coll.cljs$core$IEditableCollection$_as_transient$arity$1(coll)
  }else {
    var x__2431__auto____17219 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17220 = cljs.core._as_transient[goog.typeOf(x__2431__auto____17219)];
      if(or__3824__auto____17220) {
        return or__3824__auto____17220
      }else {
        var or__3824__auto____17221 = cljs.core._as_transient["_"];
        if(or__3824__auto____17221) {
          return or__3824__auto____17221
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
    var and__3822__auto____17226 = tcoll;
    if(and__3822__auto____17226) {
      return tcoll.cljs$core$ITransientCollection$_conj_BANG_$arity$2
    }else {
      return and__3822__auto____17226
    }
  }()) {
    return tcoll.cljs$core$ITransientCollection$_conj_BANG_$arity$2(tcoll, val)
  }else {
    var x__2431__auto____17227 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17228 = cljs.core._conj_BANG_[goog.typeOf(x__2431__auto____17227)];
      if(or__3824__auto____17228) {
        return or__3824__auto____17228
      }else {
        var or__3824__auto____17229 = cljs.core._conj_BANG_["_"];
        if(or__3824__auto____17229) {
          return or__3824__auto____17229
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientCollection.-conj!", tcoll);
        }
      }
    }().call(null, tcoll, val)
  }
};
cljs.core._persistent_BANG_ = function _persistent_BANG_(tcoll) {
  if(function() {
    var and__3822__auto____17234 = tcoll;
    if(and__3822__auto____17234) {
      return tcoll.cljs$core$ITransientCollection$_persistent_BANG_$arity$1
    }else {
      return and__3822__auto____17234
    }
  }()) {
    return tcoll.cljs$core$ITransientCollection$_persistent_BANG_$arity$1(tcoll)
  }else {
    var x__2431__auto____17235 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17236 = cljs.core._persistent_BANG_[goog.typeOf(x__2431__auto____17235)];
      if(or__3824__auto____17236) {
        return or__3824__auto____17236
      }else {
        var or__3824__auto____17237 = cljs.core._persistent_BANG_["_"];
        if(or__3824__auto____17237) {
          return or__3824__auto____17237
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
    var and__3822__auto____17242 = tcoll;
    if(and__3822__auto____17242) {
      return tcoll.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3
    }else {
      return and__3822__auto____17242
    }
  }()) {
    return tcoll.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(tcoll, key, val)
  }else {
    var x__2431__auto____17243 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17244 = cljs.core._assoc_BANG_[goog.typeOf(x__2431__auto____17243)];
      if(or__3824__auto____17244) {
        return or__3824__auto____17244
      }else {
        var or__3824__auto____17245 = cljs.core._assoc_BANG_["_"];
        if(or__3824__auto____17245) {
          return or__3824__auto____17245
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
    var and__3822__auto____17250 = tcoll;
    if(and__3822__auto____17250) {
      return tcoll.cljs$core$ITransientMap$_dissoc_BANG_$arity$2
    }else {
      return and__3822__auto____17250
    }
  }()) {
    return tcoll.cljs$core$ITransientMap$_dissoc_BANG_$arity$2(tcoll, key)
  }else {
    var x__2431__auto____17251 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17252 = cljs.core._dissoc_BANG_[goog.typeOf(x__2431__auto____17251)];
      if(or__3824__auto____17252) {
        return or__3824__auto____17252
      }else {
        var or__3824__auto____17253 = cljs.core._dissoc_BANG_["_"];
        if(or__3824__auto____17253) {
          return or__3824__auto____17253
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
    var and__3822__auto____17258 = tcoll;
    if(and__3822__auto____17258) {
      return tcoll.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3
    }else {
      return and__3822__auto____17258
    }
  }()) {
    return tcoll.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3(tcoll, n, val)
  }else {
    var x__2431__auto____17259 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17260 = cljs.core._assoc_n_BANG_[goog.typeOf(x__2431__auto____17259)];
      if(or__3824__auto____17260) {
        return or__3824__auto____17260
      }else {
        var or__3824__auto____17261 = cljs.core._assoc_n_BANG_["_"];
        if(or__3824__auto____17261) {
          return or__3824__auto____17261
        }else {
          throw cljs.core.missing_protocol.call(null, "ITransientVector.-assoc-n!", tcoll);
        }
      }
    }().call(null, tcoll, n, val)
  }
};
cljs.core._pop_BANG_ = function _pop_BANG_(tcoll) {
  if(function() {
    var and__3822__auto____17266 = tcoll;
    if(and__3822__auto____17266) {
      return tcoll.cljs$core$ITransientVector$_pop_BANG_$arity$1
    }else {
      return and__3822__auto____17266
    }
  }()) {
    return tcoll.cljs$core$ITransientVector$_pop_BANG_$arity$1(tcoll)
  }else {
    var x__2431__auto____17267 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17268 = cljs.core._pop_BANG_[goog.typeOf(x__2431__auto____17267)];
      if(or__3824__auto____17268) {
        return or__3824__auto____17268
      }else {
        var or__3824__auto____17269 = cljs.core._pop_BANG_["_"];
        if(or__3824__auto____17269) {
          return or__3824__auto____17269
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
    var and__3822__auto____17274 = tcoll;
    if(and__3822__auto____17274) {
      return tcoll.cljs$core$ITransientSet$_disjoin_BANG_$arity$2
    }else {
      return and__3822__auto____17274
    }
  }()) {
    return tcoll.cljs$core$ITransientSet$_disjoin_BANG_$arity$2(tcoll, v)
  }else {
    var x__2431__auto____17275 = tcoll == null ? null : tcoll;
    return function() {
      var or__3824__auto____17276 = cljs.core._disjoin_BANG_[goog.typeOf(x__2431__auto____17275)];
      if(or__3824__auto____17276) {
        return or__3824__auto____17276
      }else {
        var or__3824__auto____17277 = cljs.core._disjoin_BANG_["_"];
        if(or__3824__auto____17277) {
          return or__3824__auto____17277
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
    var and__3822__auto____17282 = x;
    if(and__3822__auto____17282) {
      return x.cljs$core$IComparable$_compare$arity$2
    }else {
      return and__3822__auto____17282
    }
  }()) {
    return x.cljs$core$IComparable$_compare$arity$2(x, y)
  }else {
    var x__2431__auto____17283 = x == null ? null : x;
    return function() {
      var or__3824__auto____17284 = cljs.core._compare[goog.typeOf(x__2431__auto____17283)];
      if(or__3824__auto____17284) {
        return or__3824__auto____17284
      }else {
        var or__3824__auto____17285 = cljs.core._compare["_"];
        if(or__3824__auto____17285) {
          return or__3824__auto____17285
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
    var and__3822__auto____17290 = coll;
    if(and__3822__auto____17290) {
      return coll.cljs$core$IChunk$_drop_first$arity$1
    }else {
      return and__3822__auto____17290
    }
  }()) {
    return coll.cljs$core$IChunk$_drop_first$arity$1(coll)
  }else {
    var x__2431__auto____17291 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17292 = cljs.core._drop_first[goog.typeOf(x__2431__auto____17291)];
      if(or__3824__auto____17292) {
        return or__3824__auto____17292
      }else {
        var or__3824__auto____17293 = cljs.core._drop_first["_"];
        if(or__3824__auto____17293) {
          return or__3824__auto____17293
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
    var and__3822__auto____17298 = coll;
    if(and__3822__auto____17298) {
      return coll.cljs$core$IChunkedSeq$_chunked_first$arity$1
    }else {
      return and__3822__auto____17298
    }
  }()) {
    return coll.cljs$core$IChunkedSeq$_chunked_first$arity$1(coll)
  }else {
    var x__2431__auto____17299 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17300 = cljs.core._chunked_first[goog.typeOf(x__2431__auto____17299)];
      if(or__3824__auto____17300) {
        return or__3824__auto____17300
      }else {
        var or__3824__auto____17301 = cljs.core._chunked_first["_"];
        if(or__3824__auto____17301) {
          return or__3824__auto____17301
        }else {
          throw cljs.core.missing_protocol.call(null, "IChunkedSeq.-chunked-first", coll);
        }
      }
    }().call(null, coll)
  }
};
cljs.core._chunked_rest = function _chunked_rest(coll) {
  if(function() {
    var and__3822__auto____17306 = coll;
    if(and__3822__auto____17306) {
      return coll.cljs$core$IChunkedSeq$_chunked_rest$arity$1
    }else {
      return and__3822__auto____17306
    }
  }()) {
    return coll.cljs$core$IChunkedSeq$_chunked_rest$arity$1(coll)
  }else {
    var x__2431__auto____17307 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17308 = cljs.core._chunked_rest[goog.typeOf(x__2431__auto____17307)];
      if(or__3824__auto____17308) {
        return or__3824__auto____17308
      }else {
        var or__3824__auto____17309 = cljs.core._chunked_rest["_"];
        if(or__3824__auto____17309) {
          return or__3824__auto____17309
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
    var and__3822__auto____17314 = coll;
    if(and__3822__auto____17314) {
      return coll.cljs$core$IChunkedNext$_chunked_next$arity$1
    }else {
      return and__3822__auto____17314
    }
  }()) {
    return coll.cljs$core$IChunkedNext$_chunked_next$arity$1(coll)
  }else {
    var x__2431__auto____17315 = coll == null ? null : coll;
    return function() {
      var or__3824__auto____17316 = cljs.core._chunked_next[goog.typeOf(x__2431__auto____17315)];
      if(or__3824__auto____17316) {
        return or__3824__auto____17316
      }else {
        var or__3824__auto____17317 = cljs.core._chunked_next["_"];
        if(or__3824__auto____17317) {
          return or__3824__auto____17317
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
      var G__17321__17322 = coll;
      if(G__17321__17322) {
        if(function() {
          var or__3824__auto____17323 = G__17321__17322.cljs$lang$protocol_mask$partition0$ & 32;
          if(or__3824__auto____17323) {
            return or__3824__auto____17323
          }else {
            return G__17321__17322.cljs$core$ASeq$
          }
        }()) {
          return true
        }else {
          if(!G__17321__17322.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ASeq, G__17321__17322)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ASeq, G__17321__17322)
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
      var G__17328__17329 = coll;
      if(G__17328__17329) {
        if(function() {
          var or__3824__auto____17330 = G__17328__17329.cljs$lang$protocol_mask$partition0$ & 64;
          if(or__3824__auto____17330) {
            return or__3824__auto____17330
          }else {
            return G__17328__17329.cljs$core$ISeq$
          }
        }()) {
          return true
        }else {
          if(!G__17328__17329.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17328__17329)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17328__17329)
      }
    }()) {
      return cljs.core._first.call(null, coll)
    }else {
      var s__17331 = cljs.core.seq.call(null, coll);
      if(s__17331 == null) {
        return null
      }else {
        return cljs.core._first.call(null, s__17331)
      }
    }
  }
};
cljs.core.rest = function rest(coll) {
  if(!(coll == null)) {
    if(function() {
      var G__17336__17337 = coll;
      if(G__17336__17337) {
        if(function() {
          var or__3824__auto____17338 = G__17336__17337.cljs$lang$protocol_mask$partition0$ & 64;
          if(or__3824__auto____17338) {
            return or__3824__auto____17338
          }else {
            return G__17336__17337.cljs$core$ISeq$
          }
        }()) {
          return true
        }else {
          if(!G__17336__17337.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17336__17337)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17336__17337)
      }
    }()) {
      return cljs.core._rest.call(null, coll)
    }else {
      var s__17339 = cljs.core.seq.call(null, coll);
      if(!(s__17339 == null)) {
        return cljs.core._rest.call(null, s__17339)
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
      var G__17343__17344 = coll;
      if(G__17343__17344) {
        if(function() {
          var or__3824__auto____17345 = G__17343__17344.cljs$lang$protocol_mask$partition0$ & 128;
          if(or__3824__auto____17345) {
            return or__3824__auto____17345
          }else {
            return G__17343__17344.cljs$core$INext$
          }
        }()) {
          return true
        }else {
          if(!G__17343__17344.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.INext, G__17343__17344)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.INext, G__17343__17344)
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
    var or__3824__auto____17347 = x === y;
    if(or__3824__auto____17347) {
      return or__3824__auto____17347
    }else {
      return cljs.core._equiv.call(null, x, y)
    }
  };
  var _EQ___3 = function() {
    var G__17348__delegate = function(x, y, more) {
      while(true) {
        if(cljs.core.truth_(_EQ_.call(null, x, y))) {
          if(cljs.core.next.call(null, more)) {
            var G__17349 = y;
            var G__17350 = cljs.core.first.call(null, more);
            var G__17351 = cljs.core.next.call(null, more);
            x = G__17349;
            y = G__17350;
            more = G__17351;
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
    var G__17348 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17348__delegate.call(this, x, y, more)
    };
    G__17348.cljs$lang$maxFixedArity = 2;
    G__17348.cljs$lang$applyTo = function(arglist__17352) {
      var x = cljs.core.first(arglist__17352);
      var y = cljs.core.first(cljs.core.next(arglist__17352));
      var more = cljs.core.rest(cljs.core.next(arglist__17352));
      return G__17348__delegate(x, y, more)
    };
    G__17348.cljs$lang$arity$variadic = G__17348__delegate;
    return G__17348
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
  var G__17353 = null;
  var G__17353__2 = function(o, k) {
    return null
  };
  var G__17353__3 = function(o, k, not_found) {
    return not_found
  };
  G__17353 = function(o, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17353__2.call(this, o, k);
      case 3:
        return G__17353__3.call(this, o, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17353
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
  var G__17354 = null;
  var G__17354__2 = function(_, f) {
    return f.call(null)
  };
  var G__17354__3 = function(_, f, start) {
    return start
  };
  G__17354 = function(_, f, start) {
    switch(arguments.length) {
      case 2:
        return G__17354__2.call(this, _, f);
      case 3:
        return G__17354__3.call(this, _, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17354
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
  var G__17355 = null;
  var G__17355__2 = function(_, n) {
    return null
  };
  var G__17355__3 = function(_, n, not_found) {
    return not_found
  };
  G__17355 = function(_, n, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17355__2.call(this, _, n);
      case 3:
        return G__17355__3.call(this, _, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17355
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
  var and__3822__auto____17356 = cljs.core.instance_QMARK_.call(null, Date, other);
  if(and__3822__auto____17356) {
    return o.toString() === other.toString()
  }else {
    return and__3822__auto____17356
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
  var this__17357 = this;
  return this__17357.val
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
    var cnt__17370 = cljs.core._count.call(null, cicoll);
    if(cnt__17370 === 0) {
      return f.call(null)
    }else {
      var val__17371 = cljs.core._nth.call(null, cicoll, 0);
      var n__17372 = 1;
      while(true) {
        if(n__17372 < cnt__17370) {
          var nval__17373 = f.call(null, val__17371, cljs.core._nth.call(null, cicoll, n__17372));
          if(cljs.core.reduced_QMARK_.call(null, nval__17373)) {
            return cljs.core.deref.call(null, nval__17373)
          }else {
            var G__17382 = nval__17373;
            var G__17383 = n__17372 + 1;
            val__17371 = G__17382;
            n__17372 = G__17383;
            continue
          }
        }else {
          return val__17371
        }
        break
      }
    }
  };
  var ci_reduce__3 = function(cicoll, f, val) {
    var cnt__17374 = cljs.core._count.call(null, cicoll);
    var val__17375 = val;
    var n__17376 = 0;
    while(true) {
      if(n__17376 < cnt__17374) {
        var nval__17377 = f.call(null, val__17375, cljs.core._nth.call(null, cicoll, n__17376));
        if(cljs.core.reduced_QMARK_.call(null, nval__17377)) {
          return cljs.core.deref.call(null, nval__17377)
        }else {
          var G__17384 = nval__17377;
          var G__17385 = n__17376 + 1;
          val__17375 = G__17384;
          n__17376 = G__17385;
          continue
        }
      }else {
        return val__17375
      }
      break
    }
  };
  var ci_reduce__4 = function(cicoll, f, val, idx) {
    var cnt__17378 = cljs.core._count.call(null, cicoll);
    var val__17379 = val;
    var n__17380 = idx;
    while(true) {
      if(n__17380 < cnt__17378) {
        var nval__17381 = f.call(null, val__17379, cljs.core._nth.call(null, cicoll, n__17380));
        if(cljs.core.reduced_QMARK_.call(null, nval__17381)) {
          return cljs.core.deref.call(null, nval__17381)
        }else {
          var G__17386 = nval__17381;
          var G__17387 = n__17380 + 1;
          val__17379 = G__17386;
          n__17380 = G__17387;
          continue
        }
      }else {
        return val__17379
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
    var cnt__17400 = arr.length;
    if(arr.length === 0) {
      return f.call(null)
    }else {
      var val__17401 = arr[0];
      var n__17402 = 1;
      while(true) {
        if(n__17402 < cnt__17400) {
          var nval__17403 = f.call(null, val__17401, arr[n__17402]);
          if(cljs.core.reduced_QMARK_.call(null, nval__17403)) {
            return cljs.core.deref.call(null, nval__17403)
          }else {
            var G__17412 = nval__17403;
            var G__17413 = n__17402 + 1;
            val__17401 = G__17412;
            n__17402 = G__17413;
            continue
          }
        }else {
          return val__17401
        }
        break
      }
    }
  };
  var array_reduce__3 = function(arr, f, val) {
    var cnt__17404 = arr.length;
    var val__17405 = val;
    var n__17406 = 0;
    while(true) {
      if(n__17406 < cnt__17404) {
        var nval__17407 = f.call(null, val__17405, arr[n__17406]);
        if(cljs.core.reduced_QMARK_.call(null, nval__17407)) {
          return cljs.core.deref.call(null, nval__17407)
        }else {
          var G__17414 = nval__17407;
          var G__17415 = n__17406 + 1;
          val__17405 = G__17414;
          n__17406 = G__17415;
          continue
        }
      }else {
        return val__17405
      }
      break
    }
  };
  var array_reduce__4 = function(arr, f, val, idx) {
    var cnt__17408 = arr.length;
    var val__17409 = val;
    var n__17410 = idx;
    while(true) {
      if(n__17410 < cnt__17408) {
        var nval__17411 = f.call(null, val__17409, arr[n__17410]);
        if(cljs.core.reduced_QMARK_.call(null, nval__17411)) {
          return cljs.core.deref.call(null, nval__17411)
        }else {
          var G__17416 = nval__17411;
          var G__17417 = n__17410 + 1;
          val__17409 = G__17416;
          n__17410 = G__17417;
          continue
        }
      }else {
        return val__17409
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
  var G__17421__17422 = x;
  if(G__17421__17422) {
    if(function() {
      var or__3824__auto____17423 = G__17421__17422.cljs$lang$protocol_mask$partition0$ & 2;
      if(or__3824__auto____17423) {
        return or__3824__auto____17423
      }else {
        return G__17421__17422.cljs$core$ICounted$
      }
    }()) {
      return true
    }else {
      if(!G__17421__17422.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.ICounted, G__17421__17422)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.ICounted, G__17421__17422)
  }
};
cljs.core.indexed_QMARK_ = function indexed_QMARK_(x) {
  var G__17427__17428 = x;
  if(G__17427__17428) {
    if(function() {
      var or__3824__auto____17429 = G__17427__17428.cljs$lang$protocol_mask$partition0$ & 16;
      if(or__3824__auto____17429) {
        return or__3824__auto____17429
      }else {
        return G__17427__17428.cljs$core$IIndexed$
      }
    }()) {
      return true
    }else {
      if(!G__17427__17428.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17427__17428)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17427__17428)
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
  var this__17430 = this;
  return cljs.core.hash_coll.call(null, coll)
};
cljs.core.IndexedSeq.prototype.cljs$core$INext$_next$arity$1 = function(_) {
  var this__17431 = this;
  if(this__17431.i + 1 < this__17431.a.length) {
    return new cljs.core.IndexedSeq(this__17431.a, this__17431.i + 1)
  }else {
    return null
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17432 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.IndexedSeq.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__17433 = this;
  var c__17434 = coll.cljs$core$ICounted$_count$arity$1(coll);
  if(c__17434 > 0) {
    return new cljs.core.RSeq(coll, c__17434 - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.IndexedSeq.prototype.toString = function() {
  var this__17435 = this;
  var this__17436 = this;
  return cljs.core.pr_str.call(null, this__17436)
};
cljs.core.IndexedSeq.prototype.cljs$core$IReduce$_reduce$arity$2 = function(coll, f) {
  var this__17437 = this;
  if(cljs.core.counted_QMARK_.call(null, this__17437.a)) {
    return cljs.core.ci_reduce.call(null, this__17437.a, f, this__17437.a[this__17437.i], this__17437.i + 1)
  }else {
    return cljs.core.ci_reduce.call(null, coll, f, this__17437.a[this__17437.i], 0)
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IReduce$_reduce$arity$3 = function(coll, f, start) {
  var this__17438 = this;
  if(cljs.core.counted_QMARK_.call(null, this__17438.a)) {
    return cljs.core.ci_reduce.call(null, this__17438.a, f, start, this__17438.i)
  }else {
    return cljs.core.ci_reduce.call(null, coll, f, start, 0)
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__17439 = this;
  return this$
};
cljs.core.IndexedSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(_) {
  var this__17440 = this;
  return this__17440.a.length - this__17440.i
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(_) {
  var this__17441 = this;
  return this__17441.a[this__17441.i]
};
cljs.core.IndexedSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(_) {
  var this__17442 = this;
  if(this__17442.i + 1 < this__17442.a.length) {
    return new cljs.core.IndexedSeq(this__17442.a, this__17442.i + 1)
  }else {
    return cljs.core.list.call(null)
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17443 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.IndexedSeq.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__17444 = this;
  var i__17445 = n + this__17444.i;
  if(i__17445 < this__17444.a.length) {
    return this__17444.a[i__17445]
  }else {
    return null
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__17446 = this;
  var i__17447 = n + this__17446.i;
  if(i__17447 < this__17446.a.length) {
    return this__17446.a[i__17447]
  }else {
    return not_found
  }
};
cljs.core.IndexedSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17448 = this;
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
  var G__17449 = null;
  var G__17449__2 = function(array, f) {
    return cljs.core.ci_reduce.call(null, array, f)
  };
  var G__17449__3 = function(array, f, start) {
    return cljs.core.ci_reduce.call(null, array, f, start)
  };
  G__17449 = function(array, f, start) {
    switch(arguments.length) {
      case 2:
        return G__17449__2.call(this, array, f);
      case 3:
        return G__17449__3.call(this, array, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17449
}();
cljs.core.ILookup["array"] = true;
cljs.core._lookup["array"] = function() {
  var G__17450 = null;
  var G__17450__2 = function(array, k) {
    return array[k]
  };
  var G__17450__3 = function(array, k, not_found) {
    return cljs.core._nth.call(null, array, k, not_found)
  };
  G__17450 = function(array, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17450__2.call(this, array, k);
      case 3:
        return G__17450__3.call(this, array, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17450
}();
cljs.core.IIndexed["array"] = true;
cljs.core._nth["array"] = function() {
  var G__17451 = null;
  var G__17451__2 = function(array, n) {
    if(n < array.length) {
      return array[n]
    }else {
      return null
    }
  };
  var G__17451__3 = function(array, n, not_found) {
    if(n < array.length) {
      return array[n]
    }else {
      return not_found
    }
  };
  G__17451 = function(array, n, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17451__2.call(this, array, n);
      case 3:
        return G__17451__3.call(this, array, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17451
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
  var this__17452 = this;
  return cljs.core.hash_coll.call(null, coll)
};
cljs.core.RSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17453 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.RSeq.prototype.toString = function() {
  var this__17454 = this;
  var this__17455 = this;
  return cljs.core.pr_str.call(null, this__17455)
};
cljs.core.RSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17456 = this;
  return coll
};
cljs.core.RSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__17457 = this;
  return this__17457.i + 1
};
cljs.core.RSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17458 = this;
  return cljs.core._nth.call(null, this__17458.ci, this__17458.i)
};
cljs.core.RSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17459 = this;
  if(this__17459.i > 0) {
    return new cljs.core.RSeq(this__17459.ci, this__17459.i - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.RSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17460 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.RSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, new_meta) {
  var this__17461 = this;
  return new cljs.core.RSeq(this__17461.ci, this__17461.i, new_meta)
};
cljs.core.RSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17462 = this;
  return this__17462.meta
};
cljs.core.RSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17463 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17463.meta)
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
    var sn__17465 = cljs.core.next.call(null, s);
    if(!(sn__17465 == null)) {
      var G__17466 = sn__17465;
      s = G__17466;
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
    var G__17467__delegate = function(coll, x, xs) {
      while(true) {
        if(cljs.core.truth_(xs)) {
          var G__17468 = conj.call(null, coll, x);
          var G__17469 = cljs.core.first.call(null, xs);
          var G__17470 = cljs.core.next.call(null, xs);
          coll = G__17468;
          x = G__17469;
          xs = G__17470;
          continue
        }else {
          return conj.call(null, coll, x)
        }
        break
      }
    };
    var G__17467 = function(coll, x, var_args) {
      var xs = null;
      if(goog.isDef(var_args)) {
        xs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17467__delegate.call(this, coll, x, xs)
    };
    G__17467.cljs$lang$maxFixedArity = 2;
    G__17467.cljs$lang$applyTo = function(arglist__17471) {
      var coll = cljs.core.first(arglist__17471);
      var x = cljs.core.first(cljs.core.next(arglist__17471));
      var xs = cljs.core.rest(cljs.core.next(arglist__17471));
      return G__17467__delegate(coll, x, xs)
    };
    G__17467.cljs$lang$arity$variadic = G__17467__delegate;
    return G__17467
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
  var s__17474 = cljs.core.seq.call(null, coll);
  var acc__17475 = 0;
  while(true) {
    if(cljs.core.counted_QMARK_.call(null, s__17474)) {
      return acc__17475 + cljs.core._count.call(null, s__17474)
    }else {
      var G__17476 = cljs.core.next.call(null, s__17474);
      var G__17477 = acc__17475 + 1;
      s__17474 = G__17476;
      acc__17475 = G__17477;
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
              var G__17478 = cljs.core.next.call(null, coll);
              var G__17479 = n - 1;
              coll = G__17478;
              n = G__17479;
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
              var G__17480 = cljs.core.next.call(null, coll);
              var G__17481 = n - 1;
              var G__17482 = not_found;
              coll = G__17480;
              n = G__17481;
              not_found = G__17482;
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
        var G__17489__17490 = coll;
        if(G__17489__17490) {
          if(function() {
            var or__3824__auto____17491 = G__17489__17490.cljs$lang$protocol_mask$partition0$ & 16;
            if(or__3824__auto____17491) {
              return or__3824__auto____17491
            }else {
              return G__17489__17490.cljs$core$IIndexed$
            }
          }()) {
            return true
          }else {
            if(!G__17489__17490.cljs$lang$protocol_mask$partition0$) {
              return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17489__17490)
            }else {
              return false
            }
          }
        }else {
          return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17489__17490)
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
        var G__17492__17493 = coll;
        if(G__17492__17493) {
          if(function() {
            var or__3824__auto____17494 = G__17492__17493.cljs$lang$protocol_mask$partition0$ & 16;
            if(or__3824__auto____17494) {
              return or__3824__auto____17494
            }else {
              return G__17492__17493.cljs$core$IIndexed$
            }
          }()) {
            return true
          }else {
            if(!G__17492__17493.cljs$lang$protocol_mask$partition0$) {
              return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17492__17493)
            }else {
              return false
            }
          }
        }else {
          return cljs.core.type_satisfies_.call(null, cljs.core.IIndexed, G__17492__17493)
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
    var G__17497__delegate = function(coll, k, v, kvs) {
      while(true) {
        var ret__17496 = assoc.call(null, coll, k, v);
        if(cljs.core.truth_(kvs)) {
          var G__17498 = ret__17496;
          var G__17499 = cljs.core.first.call(null, kvs);
          var G__17500 = cljs.core.second.call(null, kvs);
          var G__17501 = cljs.core.nnext.call(null, kvs);
          coll = G__17498;
          k = G__17499;
          v = G__17500;
          kvs = G__17501;
          continue
        }else {
          return ret__17496
        }
        break
      }
    };
    var G__17497 = function(coll, k, v, var_args) {
      var kvs = null;
      if(goog.isDef(var_args)) {
        kvs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__17497__delegate.call(this, coll, k, v, kvs)
    };
    G__17497.cljs$lang$maxFixedArity = 3;
    G__17497.cljs$lang$applyTo = function(arglist__17502) {
      var coll = cljs.core.first(arglist__17502);
      var k = cljs.core.first(cljs.core.next(arglist__17502));
      var v = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17502)));
      var kvs = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17502)));
      return G__17497__delegate(coll, k, v, kvs)
    };
    G__17497.cljs$lang$arity$variadic = G__17497__delegate;
    return G__17497
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
    var G__17505__delegate = function(coll, k, ks) {
      while(true) {
        var ret__17504 = dissoc.call(null, coll, k);
        if(cljs.core.truth_(ks)) {
          var G__17506 = ret__17504;
          var G__17507 = cljs.core.first.call(null, ks);
          var G__17508 = cljs.core.next.call(null, ks);
          coll = G__17506;
          k = G__17507;
          ks = G__17508;
          continue
        }else {
          return ret__17504
        }
        break
      }
    };
    var G__17505 = function(coll, k, var_args) {
      var ks = null;
      if(goog.isDef(var_args)) {
        ks = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17505__delegate.call(this, coll, k, ks)
    };
    G__17505.cljs$lang$maxFixedArity = 2;
    G__17505.cljs$lang$applyTo = function(arglist__17509) {
      var coll = cljs.core.first(arglist__17509);
      var k = cljs.core.first(cljs.core.next(arglist__17509));
      var ks = cljs.core.rest(cljs.core.next(arglist__17509));
      return G__17505__delegate(coll, k, ks)
    };
    G__17505.cljs$lang$arity$variadic = G__17505__delegate;
    return G__17505
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
    var G__17513__17514 = o;
    if(G__17513__17514) {
      if(function() {
        var or__3824__auto____17515 = G__17513__17514.cljs$lang$protocol_mask$partition0$ & 131072;
        if(or__3824__auto____17515) {
          return or__3824__auto____17515
        }else {
          return G__17513__17514.cljs$core$IMeta$
        }
      }()) {
        return true
      }else {
        if(!G__17513__17514.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__17513__17514)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__17513__17514)
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
    var G__17518__delegate = function(coll, k, ks) {
      while(true) {
        var ret__17517 = disj.call(null, coll, k);
        if(cljs.core.truth_(ks)) {
          var G__17519 = ret__17517;
          var G__17520 = cljs.core.first.call(null, ks);
          var G__17521 = cljs.core.next.call(null, ks);
          coll = G__17519;
          k = G__17520;
          ks = G__17521;
          continue
        }else {
          return ret__17517
        }
        break
      }
    };
    var G__17518 = function(coll, k, var_args) {
      var ks = null;
      if(goog.isDef(var_args)) {
        ks = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17518__delegate.call(this, coll, k, ks)
    };
    G__17518.cljs$lang$maxFixedArity = 2;
    G__17518.cljs$lang$applyTo = function(arglist__17522) {
      var coll = cljs.core.first(arglist__17522);
      var k = cljs.core.first(cljs.core.next(arglist__17522));
      var ks = cljs.core.rest(cljs.core.next(arglist__17522));
      return G__17518__delegate(coll, k, ks)
    };
    G__17518.cljs$lang$arity$variadic = G__17518__delegate;
    return G__17518
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
  var h__17524 = goog.string.hashCode(k);
  cljs.core.string_hash_cache[k] = h__17524;
  cljs.core.string_hash_cache_count = cljs.core.string_hash_cache_count + 1;
  return h__17524
};
cljs.core.check_string_hash_cache = function check_string_hash_cache(k) {
  if(cljs.core.string_hash_cache_count > 255) {
    cljs.core.string_hash_cache = {};
    cljs.core.string_hash_cache_count = 0
  }else {
  }
  var h__17526 = cljs.core.string_hash_cache[k];
  if(!(h__17526 == null)) {
    return h__17526
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
      var and__3822__auto____17528 = goog.isString(o);
      if(and__3822__auto____17528) {
        return check_cache
      }else {
        return and__3822__auto____17528
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
    var G__17532__17533 = x;
    if(G__17532__17533) {
      if(function() {
        var or__3824__auto____17534 = G__17532__17533.cljs$lang$protocol_mask$partition0$ & 8;
        if(or__3824__auto____17534) {
          return or__3824__auto____17534
        }else {
          return G__17532__17533.cljs$core$ICollection$
        }
      }()) {
        return true
      }else {
        if(!G__17532__17533.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.ICollection, G__17532__17533)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.ICollection, G__17532__17533)
    }
  }
};
cljs.core.set_QMARK_ = function set_QMARK_(x) {
  if(x == null) {
    return false
  }else {
    var G__17538__17539 = x;
    if(G__17538__17539) {
      if(function() {
        var or__3824__auto____17540 = G__17538__17539.cljs$lang$protocol_mask$partition0$ & 4096;
        if(or__3824__auto____17540) {
          return or__3824__auto____17540
        }else {
          return G__17538__17539.cljs$core$ISet$
        }
      }()) {
        return true
      }else {
        if(!G__17538__17539.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.ISet, G__17538__17539)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.ISet, G__17538__17539)
    }
  }
};
cljs.core.associative_QMARK_ = function associative_QMARK_(x) {
  var G__17544__17545 = x;
  if(G__17544__17545) {
    if(function() {
      var or__3824__auto____17546 = G__17544__17545.cljs$lang$protocol_mask$partition0$ & 512;
      if(or__3824__auto____17546) {
        return or__3824__auto____17546
      }else {
        return G__17544__17545.cljs$core$IAssociative$
      }
    }()) {
      return true
    }else {
      if(!G__17544__17545.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IAssociative, G__17544__17545)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IAssociative, G__17544__17545)
  }
};
cljs.core.sequential_QMARK_ = function sequential_QMARK_(x) {
  var G__17550__17551 = x;
  if(G__17550__17551) {
    if(function() {
      var or__3824__auto____17552 = G__17550__17551.cljs$lang$protocol_mask$partition0$ & 16777216;
      if(or__3824__auto____17552) {
        return or__3824__auto____17552
      }else {
        return G__17550__17551.cljs$core$ISequential$
      }
    }()) {
      return true
    }else {
      if(!G__17550__17551.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISequential, G__17550__17551)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.ISequential, G__17550__17551)
  }
};
cljs.core.reduceable_QMARK_ = function reduceable_QMARK_(x) {
  var G__17556__17557 = x;
  if(G__17556__17557) {
    if(function() {
      var or__3824__auto____17558 = G__17556__17557.cljs$lang$protocol_mask$partition0$ & 524288;
      if(or__3824__auto____17558) {
        return or__3824__auto____17558
      }else {
        return G__17556__17557.cljs$core$IReduce$
      }
    }()) {
      return true
    }else {
      if(!G__17556__17557.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17556__17557)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17556__17557)
  }
};
cljs.core.map_QMARK_ = function map_QMARK_(x) {
  if(x == null) {
    return false
  }else {
    var G__17562__17563 = x;
    if(G__17562__17563) {
      if(function() {
        var or__3824__auto____17564 = G__17562__17563.cljs$lang$protocol_mask$partition0$ & 1024;
        if(or__3824__auto____17564) {
          return or__3824__auto____17564
        }else {
          return G__17562__17563.cljs$core$IMap$
        }
      }()) {
        return true
      }else {
        if(!G__17562__17563.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IMap, G__17562__17563)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IMap, G__17562__17563)
    }
  }
};
cljs.core.vector_QMARK_ = function vector_QMARK_(x) {
  var G__17568__17569 = x;
  if(G__17568__17569) {
    if(function() {
      var or__3824__auto____17570 = G__17568__17569.cljs$lang$protocol_mask$partition0$ & 16384;
      if(or__3824__auto____17570) {
        return or__3824__auto____17570
      }else {
        return G__17568__17569.cljs$core$IVector$
      }
    }()) {
      return true
    }else {
      if(!G__17568__17569.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IVector, G__17568__17569)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IVector, G__17568__17569)
  }
};
cljs.core.chunked_seq_QMARK_ = function chunked_seq_QMARK_(x) {
  var G__17574__17575 = x;
  if(G__17574__17575) {
    if(function() {
      var or__3824__auto____17576 = G__17574__17575.cljs$lang$protocol_mask$partition1$ & 512;
      if(or__3824__auto____17576) {
        return or__3824__auto____17576
      }else {
        return G__17574__17575.cljs$core$IChunkedSeq$
      }
    }()) {
      return true
    }else {
      if(!G__17574__17575.cljs$lang$protocol_mask$partition1$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedSeq, G__17574__17575)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedSeq, G__17574__17575)
  }
};
cljs.core.js_obj = function() {
  var js_obj = null;
  var js_obj__0 = function() {
    return{}
  };
  var js_obj__1 = function() {
    var G__17577__delegate = function(keyvals) {
      return cljs.core.apply.call(null, goog.object.create, keyvals)
    };
    var G__17577 = function(var_args) {
      var keyvals = null;
      if(goog.isDef(var_args)) {
        keyvals = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__17577__delegate.call(this, keyvals)
    };
    G__17577.cljs$lang$maxFixedArity = 0;
    G__17577.cljs$lang$applyTo = function(arglist__17578) {
      var keyvals = cljs.core.seq(arglist__17578);
      return G__17577__delegate(keyvals)
    };
    G__17577.cljs$lang$arity$variadic = G__17577__delegate;
    return G__17577
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
  var keys__17580 = [];
  goog.object.forEach(obj, function(val, key, obj) {
    return keys__17580.push(key)
  });
  return keys__17580
};
cljs.core.js_delete = function js_delete(obj, key) {
  return delete obj[key]
};
cljs.core.array_copy = function array_copy(from, i, to, j, len) {
  var i__17584 = i;
  var j__17585 = j;
  var len__17586 = len;
  while(true) {
    if(len__17586 === 0) {
      return to
    }else {
      to[j__17585] = from[i__17584];
      var G__17587 = i__17584 + 1;
      var G__17588 = j__17585 + 1;
      var G__17589 = len__17586 - 1;
      i__17584 = G__17587;
      j__17585 = G__17588;
      len__17586 = G__17589;
      continue
    }
    break
  }
};
cljs.core.array_copy_downward = function array_copy_downward(from, i, to, j, len) {
  var i__17593 = i + (len - 1);
  var j__17594 = j + (len - 1);
  var len__17595 = len;
  while(true) {
    if(len__17595 === 0) {
      return to
    }else {
      to[j__17594] = from[i__17593];
      var G__17596 = i__17593 - 1;
      var G__17597 = j__17594 - 1;
      var G__17598 = len__17595 - 1;
      i__17593 = G__17596;
      j__17594 = G__17597;
      len__17595 = G__17598;
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
    var G__17602__17603 = s;
    if(G__17602__17603) {
      if(function() {
        var or__3824__auto____17604 = G__17602__17603.cljs$lang$protocol_mask$partition0$ & 64;
        if(or__3824__auto____17604) {
          return or__3824__auto____17604
        }else {
          return G__17602__17603.cljs$core$ISeq$
        }
      }()) {
        return true
      }else {
        if(!G__17602__17603.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17602__17603)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17602__17603)
    }
  }
};
cljs.core.seqable_QMARK_ = function seqable_QMARK_(s) {
  var G__17608__17609 = s;
  if(G__17608__17609) {
    if(function() {
      var or__3824__auto____17610 = G__17608__17609.cljs$lang$protocol_mask$partition0$ & 8388608;
      if(or__3824__auto____17610) {
        return or__3824__auto____17610
      }else {
        return G__17608__17609.cljs$core$ISeqable$
      }
    }()) {
      return true
    }else {
      if(!G__17608__17609.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeqable, G__17608__17609)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.ISeqable, G__17608__17609)
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
  var and__3822__auto____17613 = goog.isString(x);
  if(and__3822__auto____17613) {
    return!function() {
      var or__3824__auto____17614 = x.charAt(0) === "\ufdd0";
      if(or__3824__auto____17614) {
        return or__3824__auto____17614
      }else {
        return x.charAt(0) === "\ufdd1"
      }
    }()
  }else {
    return and__3822__auto____17613
  }
};
cljs.core.keyword_QMARK_ = function keyword_QMARK_(x) {
  var and__3822__auto____17616 = goog.isString(x);
  if(and__3822__auto____17616) {
    return x.charAt(0) === "\ufdd0"
  }else {
    return and__3822__auto____17616
  }
};
cljs.core.symbol_QMARK_ = function symbol_QMARK_(x) {
  var and__3822__auto____17618 = goog.isString(x);
  if(and__3822__auto____17618) {
    return x.charAt(0) === "\ufdd1"
  }else {
    return and__3822__auto____17618
  }
};
cljs.core.number_QMARK_ = function number_QMARK_(n) {
  return goog.isNumber(n)
};
cljs.core.fn_QMARK_ = function fn_QMARK_(f) {
  return goog.isFunction(f)
};
cljs.core.ifn_QMARK_ = function ifn_QMARK_(f) {
  var or__3824__auto____17623 = cljs.core.fn_QMARK_.call(null, f);
  if(or__3824__auto____17623) {
    return or__3824__auto____17623
  }else {
    var G__17624__17625 = f;
    if(G__17624__17625) {
      if(function() {
        var or__3824__auto____17626 = G__17624__17625.cljs$lang$protocol_mask$partition0$ & 1;
        if(or__3824__auto____17626) {
          return or__3824__auto____17626
        }else {
          return G__17624__17625.cljs$core$IFn$
        }
      }()) {
        return true
      }else {
        if(!G__17624__17625.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IFn, G__17624__17625)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IFn, G__17624__17625)
    }
  }
};
cljs.core.integer_QMARK_ = function integer_QMARK_(n) {
  var and__3822__auto____17630 = cljs.core.number_QMARK_.call(null, n);
  if(and__3822__auto____17630) {
    var and__3822__auto____17631 = !isNaN(n);
    if(and__3822__auto____17631) {
      var and__3822__auto____17632 = !(n === Infinity);
      if(and__3822__auto____17632) {
        return parseFloat(n) === parseInt(n, 10)
      }else {
        return and__3822__auto____17632
      }
    }else {
      return and__3822__auto____17631
    }
  }else {
    return and__3822__auto____17630
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
    var and__3822__auto____17635 = !(coll == null);
    if(and__3822__auto____17635) {
      var and__3822__auto____17636 = cljs.core.associative_QMARK_.call(null, coll);
      if(and__3822__auto____17636) {
        return cljs.core.contains_QMARK_.call(null, coll, k)
      }else {
        return and__3822__auto____17636
      }
    }else {
      return and__3822__auto____17635
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
    var G__17645__delegate = function(x, y, more) {
      if(!cljs.core._EQ_.call(null, x, y)) {
        var s__17641 = cljs.core.PersistentHashSet.fromArray([y, x]);
        var xs__17642 = more;
        while(true) {
          var x__17643 = cljs.core.first.call(null, xs__17642);
          var etc__17644 = cljs.core.next.call(null, xs__17642);
          if(cljs.core.truth_(xs__17642)) {
            if(cljs.core.contains_QMARK_.call(null, s__17641, x__17643)) {
              return false
            }else {
              var G__17646 = cljs.core.conj.call(null, s__17641, x__17643);
              var G__17647 = etc__17644;
              s__17641 = G__17646;
              xs__17642 = G__17647;
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
    var G__17645 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17645__delegate.call(this, x, y, more)
    };
    G__17645.cljs$lang$maxFixedArity = 2;
    G__17645.cljs$lang$applyTo = function(arglist__17648) {
      var x = cljs.core.first(arglist__17648);
      var y = cljs.core.first(cljs.core.next(arglist__17648));
      var more = cljs.core.rest(cljs.core.next(arglist__17648));
      return G__17645__delegate(x, y, more)
    };
    G__17645.cljs$lang$arity$variadic = G__17645__delegate;
    return G__17645
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
            var G__17652__17653 = x;
            if(G__17652__17653) {
              if(function() {
                var or__3824__auto____17654 = G__17652__17653.cljs$lang$protocol_mask$partition1$ & 2048;
                if(or__3824__auto____17654) {
                  return or__3824__auto____17654
                }else {
                  return G__17652__17653.cljs$core$IComparable$
                }
              }()) {
                return true
              }else {
                if(!G__17652__17653.cljs$lang$protocol_mask$partition1$) {
                  return cljs.core.type_satisfies_.call(null, cljs.core.IComparable, G__17652__17653)
                }else {
                  return false
                }
              }
            }else {
              return cljs.core.type_satisfies_.call(null, cljs.core.IComparable, G__17652__17653)
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
    var xl__17659 = cljs.core.count.call(null, xs);
    var yl__17660 = cljs.core.count.call(null, ys);
    if(xl__17659 < yl__17660) {
      return-1
    }else {
      if(xl__17659 > yl__17660) {
        return 1
      }else {
        if("\ufdd0'else") {
          return compare_indexed.call(null, xs, ys, xl__17659, 0)
        }else {
          return null
        }
      }
    }
  };
  var compare_indexed__4 = function(xs, ys, len, n) {
    while(true) {
      var d__17661 = cljs.core.compare.call(null, cljs.core.nth.call(null, xs, n), cljs.core.nth.call(null, ys, n));
      if(function() {
        var and__3822__auto____17662 = d__17661 === 0;
        if(and__3822__auto____17662) {
          return n + 1 < len
        }else {
          return and__3822__auto____17662
        }
      }()) {
        var G__17663 = xs;
        var G__17664 = ys;
        var G__17665 = len;
        var G__17666 = n + 1;
        xs = G__17663;
        ys = G__17664;
        len = G__17665;
        n = G__17666;
        continue
      }else {
        return d__17661
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
      var r__17668 = f.call(null, x, y);
      if(cljs.core.number_QMARK_.call(null, r__17668)) {
        return r__17668
      }else {
        if(cljs.core.truth_(r__17668)) {
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
      var a__17670 = cljs.core.to_array.call(null, coll);
      goog.array.stableSort(a__17670, cljs.core.fn__GT_comparator.call(null, comp));
      return cljs.core.seq.call(null, a__17670)
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
    var temp__3971__auto____17676 = cljs.core.seq.call(null, coll);
    if(temp__3971__auto____17676) {
      var s__17677 = temp__3971__auto____17676;
      return cljs.core.reduce.call(null, f, cljs.core.first.call(null, s__17677), cljs.core.next.call(null, s__17677))
    }else {
      return f.call(null)
    }
  };
  var seq_reduce__3 = function(f, val, coll) {
    var val__17678 = val;
    var coll__17679 = cljs.core.seq.call(null, coll);
    while(true) {
      if(coll__17679) {
        var nval__17680 = f.call(null, val__17678, cljs.core.first.call(null, coll__17679));
        if(cljs.core.reduced_QMARK_.call(null, nval__17680)) {
          return cljs.core.deref.call(null, nval__17680)
        }else {
          var G__17681 = nval__17680;
          var G__17682 = cljs.core.next.call(null, coll__17679);
          val__17678 = G__17681;
          coll__17679 = G__17682;
          continue
        }
      }else {
        return val__17678
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
  var a__17684 = cljs.core.to_array.call(null, coll);
  goog.array.shuffle(a__17684);
  return cljs.core.vec.call(null, a__17684)
};
cljs.core.reduce = function() {
  var reduce = null;
  var reduce__2 = function(f, coll) {
    if(function() {
      var G__17691__17692 = coll;
      if(G__17691__17692) {
        if(function() {
          var or__3824__auto____17693 = G__17691__17692.cljs$lang$protocol_mask$partition0$ & 524288;
          if(or__3824__auto____17693) {
            return or__3824__auto____17693
          }else {
            return G__17691__17692.cljs$core$IReduce$
          }
        }()) {
          return true
        }else {
          if(!G__17691__17692.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17691__17692)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17691__17692)
      }
    }()) {
      return cljs.core._reduce.call(null, coll, f)
    }else {
      return cljs.core.seq_reduce.call(null, f, coll)
    }
  };
  var reduce__3 = function(f, val, coll) {
    if(function() {
      var G__17694__17695 = coll;
      if(G__17694__17695) {
        if(function() {
          var or__3824__auto____17696 = G__17694__17695.cljs$lang$protocol_mask$partition0$ & 524288;
          if(or__3824__auto____17696) {
            return or__3824__auto____17696
          }else {
            return G__17694__17695.cljs$core$IReduce$
          }
        }()) {
          return true
        }else {
          if(!G__17694__17695.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17694__17695)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReduce, G__17694__17695)
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
    var G__17697__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _PLUS_, x + y, more)
    };
    var G__17697 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17697__delegate.call(this, x, y, more)
    };
    G__17697.cljs$lang$maxFixedArity = 2;
    G__17697.cljs$lang$applyTo = function(arglist__17698) {
      var x = cljs.core.first(arglist__17698);
      var y = cljs.core.first(cljs.core.next(arglist__17698));
      var more = cljs.core.rest(cljs.core.next(arglist__17698));
      return G__17697__delegate(x, y, more)
    };
    G__17697.cljs$lang$arity$variadic = G__17697__delegate;
    return G__17697
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
    var G__17699__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _, x - y, more)
    };
    var G__17699 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17699__delegate.call(this, x, y, more)
    };
    G__17699.cljs$lang$maxFixedArity = 2;
    G__17699.cljs$lang$applyTo = function(arglist__17700) {
      var x = cljs.core.first(arglist__17700);
      var y = cljs.core.first(cljs.core.next(arglist__17700));
      var more = cljs.core.rest(cljs.core.next(arglist__17700));
      return G__17699__delegate(x, y, more)
    };
    G__17699.cljs$lang$arity$variadic = G__17699__delegate;
    return G__17699
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
    var G__17701__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _STAR_, x * y, more)
    };
    var G__17701 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17701__delegate.call(this, x, y, more)
    };
    G__17701.cljs$lang$maxFixedArity = 2;
    G__17701.cljs$lang$applyTo = function(arglist__17702) {
      var x = cljs.core.first(arglist__17702);
      var y = cljs.core.first(cljs.core.next(arglist__17702));
      var more = cljs.core.rest(cljs.core.next(arglist__17702));
      return G__17701__delegate(x, y, more)
    };
    G__17701.cljs$lang$arity$variadic = G__17701__delegate;
    return G__17701
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
    var G__17703__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, _SLASH_, _SLASH_.call(null, x, y), more)
    };
    var G__17703 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17703__delegate.call(this, x, y, more)
    };
    G__17703.cljs$lang$maxFixedArity = 2;
    G__17703.cljs$lang$applyTo = function(arglist__17704) {
      var x = cljs.core.first(arglist__17704);
      var y = cljs.core.first(cljs.core.next(arglist__17704));
      var more = cljs.core.rest(cljs.core.next(arglist__17704));
      return G__17703__delegate(x, y, more)
    };
    G__17703.cljs$lang$arity$variadic = G__17703__delegate;
    return G__17703
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
    var G__17705__delegate = function(x, y, more) {
      while(true) {
        if(x < y) {
          if(cljs.core.next.call(null, more)) {
            var G__17706 = y;
            var G__17707 = cljs.core.first.call(null, more);
            var G__17708 = cljs.core.next.call(null, more);
            x = G__17706;
            y = G__17707;
            more = G__17708;
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
    var G__17705 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17705__delegate.call(this, x, y, more)
    };
    G__17705.cljs$lang$maxFixedArity = 2;
    G__17705.cljs$lang$applyTo = function(arglist__17709) {
      var x = cljs.core.first(arglist__17709);
      var y = cljs.core.first(cljs.core.next(arglist__17709));
      var more = cljs.core.rest(cljs.core.next(arglist__17709));
      return G__17705__delegate(x, y, more)
    };
    G__17705.cljs$lang$arity$variadic = G__17705__delegate;
    return G__17705
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
    var G__17710__delegate = function(x, y, more) {
      while(true) {
        if(x <= y) {
          if(cljs.core.next.call(null, more)) {
            var G__17711 = y;
            var G__17712 = cljs.core.first.call(null, more);
            var G__17713 = cljs.core.next.call(null, more);
            x = G__17711;
            y = G__17712;
            more = G__17713;
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
    var G__17710 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17710__delegate.call(this, x, y, more)
    };
    G__17710.cljs$lang$maxFixedArity = 2;
    G__17710.cljs$lang$applyTo = function(arglist__17714) {
      var x = cljs.core.first(arglist__17714);
      var y = cljs.core.first(cljs.core.next(arglist__17714));
      var more = cljs.core.rest(cljs.core.next(arglist__17714));
      return G__17710__delegate(x, y, more)
    };
    G__17710.cljs$lang$arity$variadic = G__17710__delegate;
    return G__17710
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
    var G__17715__delegate = function(x, y, more) {
      while(true) {
        if(x > y) {
          if(cljs.core.next.call(null, more)) {
            var G__17716 = y;
            var G__17717 = cljs.core.first.call(null, more);
            var G__17718 = cljs.core.next.call(null, more);
            x = G__17716;
            y = G__17717;
            more = G__17718;
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
    var G__17715 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17715__delegate.call(this, x, y, more)
    };
    G__17715.cljs$lang$maxFixedArity = 2;
    G__17715.cljs$lang$applyTo = function(arglist__17719) {
      var x = cljs.core.first(arglist__17719);
      var y = cljs.core.first(cljs.core.next(arglist__17719));
      var more = cljs.core.rest(cljs.core.next(arglist__17719));
      return G__17715__delegate(x, y, more)
    };
    G__17715.cljs$lang$arity$variadic = G__17715__delegate;
    return G__17715
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
    var G__17720__delegate = function(x, y, more) {
      while(true) {
        if(x >= y) {
          if(cljs.core.next.call(null, more)) {
            var G__17721 = y;
            var G__17722 = cljs.core.first.call(null, more);
            var G__17723 = cljs.core.next.call(null, more);
            x = G__17721;
            y = G__17722;
            more = G__17723;
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
    var G__17720 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17720__delegate.call(this, x, y, more)
    };
    G__17720.cljs$lang$maxFixedArity = 2;
    G__17720.cljs$lang$applyTo = function(arglist__17724) {
      var x = cljs.core.first(arglist__17724);
      var y = cljs.core.first(cljs.core.next(arglist__17724));
      var more = cljs.core.rest(cljs.core.next(arglist__17724));
      return G__17720__delegate(x, y, more)
    };
    G__17720.cljs$lang$arity$variadic = G__17720__delegate;
    return G__17720
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
    var G__17725__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, max, x > y ? x : y, more)
    };
    var G__17725 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17725__delegate.call(this, x, y, more)
    };
    G__17725.cljs$lang$maxFixedArity = 2;
    G__17725.cljs$lang$applyTo = function(arglist__17726) {
      var x = cljs.core.first(arglist__17726);
      var y = cljs.core.first(cljs.core.next(arglist__17726));
      var more = cljs.core.rest(cljs.core.next(arglist__17726));
      return G__17725__delegate(x, y, more)
    };
    G__17725.cljs$lang$arity$variadic = G__17725__delegate;
    return G__17725
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
    var G__17727__delegate = function(x, y, more) {
      return cljs.core.reduce.call(null, min, x < y ? x : y, more)
    };
    var G__17727 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17727__delegate.call(this, x, y, more)
    };
    G__17727.cljs$lang$maxFixedArity = 2;
    G__17727.cljs$lang$applyTo = function(arglist__17728) {
      var x = cljs.core.first(arglist__17728);
      var y = cljs.core.first(cljs.core.next(arglist__17728));
      var more = cljs.core.rest(cljs.core.next(arglist__17728));
      return G__17727__delegate(x, y, more)
    };
    G__17727.cljs$lang$arity$variadic = G__17727__delegate;
    return G__17727
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
  var rem__17730 = n % d;
  return cljs.core.fix.call(null, (n - rem__17730) / d)
};
cljs.core.rem = function rem(n, d) {
  var q__17732 = cljs.core.quot.call(null, n, d);
  return n - d * q__17732
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
  var v__17735 = v - (v >> 1 & 1431655765);
  var v__17736 = (v__17735 & 858993459) + (v__17735 >> 2 & 858993459);
  return(v__17736 + (v__17736 >> 4) & 252645135) * 16843009 >> 24
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
    var G__17737__delegate = function(x, y, more) {
      while(true) {
        if(cljs.core.truth_(_EQ__EQ_.call(null, x, y))) {
          if(cljs.core.next.call(null, more)) {
            var G__17738 = y;
            var G__17739 = cljs.core.first.call(null, more);
            var G__17740 = cljs.core.next.call(null, more);
            x = G__17738;
            y = G__17739;
            more = G__17740;
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
    var G__17737 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__17737__delegate.call(this, x, y, more)
    };
    G__17737.cljs$lang$maxFixedArity = 2;
    G__17737.cljs$lang$applyTo = function(arglist__17741) {
      var x = cljs.core.first(arglist__17741);
      var y = cljs.core.first(cljs.core.next(arglist__17741));
      var more = cljs.core.rest(cljs.core.next(arglist__17741));
      return G__17737__delegate(x, y, more)
    };
    G__17737.cljs$lang$arity$variadic = G__17737__delegate;
    return G__17737
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
  var n__17745 = n;
  var xs__17746 = cljs.core.seq.call(null, coll);
  while(true) {
    if(cljs.core.truth_(function() {
      var and__3822__auto____17747 = xs__17746;
      if(and__3822__auto____17747) {
        return n__17745 > 0
      }else {
        return and__3822__auto____17747
      }
    }())) {
      var G__17748 = n__17745 - 1;
      var G__17749 = cljs.core.next.call(null, xs__17746);
      n__17745 = G__17748;
      xs__17746 = G__17749;
      continue
    }else {
      return xs__17746
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
    var G__17750__delegate = function(x, ys) {
      return function(sb, more) {
        while(true) {
          if(cljs.core.truth_(more)) {
            var G__17751 = sb.append(str_STAR_.call(null, cljs.core.first.call(null, more)));
            var G__17752 = cljs.core.next.call(null, more);
            sb = G__17751;
            more = G__17752;
            continue
          }else {
            return str_STAR_.call(null, sb)
          }
          break
        }
      }.call(null, new goog.string.StringBuffer(str_STAR_.call(null, x)), ys)
    };
    var G__17750 = function(x, var_args) {
      var ys = null;
      if(goog.isDef(var_args)) {
        ys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__17750__delegate.call(this, x, ys)
    };
    G__17750.cljs$lang$maxFixedArity = 1;
    G__17750.cljs$lang$applyTo = function(arglist__17753) {
      var x = cljs.core.first(arglist__17753);
      var ys = cljs.core.rest(arglist__17753);
      return G__17750__delegate(x, ys)
    };
    G__17750.cljs$lang$arity$variadic = G__17750__delegate;
    return G__17750
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
    var G__17754__delegate = function(x, ys) {
      return function(sb, more) {
        while(true) {
          if(cljs.core.truth_(more)) {
            var G__17755 = sb.append(str.call(null, cljs.core.first.call(null, more)));
            var G__17756 = cljs.core.next.call(null, more);
            sb = G__17755;
            more = G__17756;
            continue
          }else {
            return cljs.core.str_STAR_.call(null, sb)
          }
          break
        }
      }.call(null, new goog.string.StringBuffer(str.call(null, x)), ys)
    };
    var G__17754 = function(x, var_args) {
      var ys = null;
      if(goog.isDef(var_args)) {
        ys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__17754__delegate.call(this, x, ys)
    };
    G__17754.cljs$lang$maxFixedArity = 1;
    G__17754.cljs$lang$applyTo = function(arglist__17757) {
      var x = cljs.core.first(arglist__17757);
      var ys = cljs.core.rest(arglist__17757);
      return G__17754__delegate(x, ys)
    };
    G__17754.cljs$lang$arity$variadic = G__17754__delegate;
    return G__17754
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
    var args__17761 = cljs.core.map.call(null, function(x) {
      if(function() {
        var or__3824__auto____17760 = cljs.core.keyword_QMARK_.call(null, x);
        if(or__3824__auto____17760) {
          return or__3824__auto____17760
        }else {
          return cljs.core.symbol_QMARK_.call(null, x)
        }
      }()) {
        return[cljs.core.str(x)].join("")
      }else {
        return x
      }
    }, args);
    return cljs.core.apply.call(null, goog.string.format, fmt, args__17761)
  };
  var format = function(fmt, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return format__delegate.call(this, fmt, args)
  };
  format.cljs$lang$maxFixedArity = 1;
  format.cljs$lang$applyTo = function(arglist__17762) {
    var fmt = cljs.core.first(arglist__17762);
    var args = cljs.core.rest(arglist__17762);
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
    var xs__17765 = cljs.core.seq.call(null, x);
    var ys__17766 = cljs.core.seq.call(null, y);
    while(true) {
      if(xs__17765 == null) {
        return ys__17766 == null
      }else {
        if(ys__17766 == null) {
          return false
        }else {
          if(cljs.core._EQ_.call(null, cljs.core.first.call(null, xs__17765), cljs.core.first.call(null, ys__17766))) {
            var G__17767 = cljs.core.next.call(null, xs__17765);
            var G__17768 = cljs.core.next.call(null, ys__17766);
            xs__17765 = G__17767;
            ys__17766 = G__17768;
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
  return cljs.core.reduce.call(null, function(p1__17769_SHARP_, p2__17770_SHARP_) {
    return cljs.core.hash_combine.call(null, p1__17769_SHARP_, cljs.core.hash.call(null, p2__17770_SHARP_, false))
  }, cljs.core.hash.call(null, cljs.core.first.call(null, coll), false), cljs.core.next.call(null, coll))
};
cljs.core.hash_imap = function hash_imap(m) {
  var h__17774 = 0;
  var s__17775 = cljs.core.seq.call(null, m);
  while(true) {
    if(s__17775) {
      var e__17776 = cljs.core.first.call(null, s__17775);
      var G__17777 = (h__17774 + (cljs.core.hash.call(null, cljs.core.key.call(null, e__17776)) ^ cljs.core.hash.call(null, cljs.core.val.call(null, e__17776)))) % 4503599627370496;
      var G__17778 = cljs.core.next.call(null, s__17775);
      h__17774 = G__17777;
      s__17775 = G__17778;
      continue
    }else {
      return h__17774
    }
    break
  }
};
cljs.core.hash_iset = function hash_iset(s) {
  var h__17782 = 0;
  var s__17783 = cljs.core.seq.call(null, s);
  while(true) {
    if(s__17783) {
      var e__17784 = cljs.core.first.call(null, s__17783);
      var G__17785 = (h__17782 + cljs.core.hash.call(null, e__17784)) % 4503599627370496;
      var G__17786 = cljs.core.next.call(null, s__17783);
      h__17782 = G__17785;
      s__17783 = G__17786;
      continue
    }else {
      return h__17782
    }
    break
  }
};
cljs.core.extend_object_BANG_ = function extend_object_BANG_(obj, fn_map) {
  var G__17794__17795 = cljs.core.seq.call(null, fn_map);
  while(true) {
    if(G__17794__17795) {
      var vec__17796__17797 = cljs.core.first.call(null, G__17794__17795);
      var key_name__17798 = cljs.core.nth.call(null, vec__17796__17797, 0, null);
      var f__17799 = cljs.core.nth.call(null, vec__17796__17797, 1, null);
      var str_name__17800 = cljs.core.name.call(null, key_name__17798);
      obj[str_name__17800] = f__17799;
      var G__17801 = cljs.core.next.call(null, G__17794__17795);
      G__17794__17795 = G__17801;
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
  var this__17802 = this;
  var h__2247__auto____17803 = this__17802.__hash;
  if(!(h__2247__auto____17803 == null)) {
    return h__2247__auto____17803
  }else {
    var h__2247__auto____17804 = cljs.core.hash_coll.call(null, coll);
    this__17802.__hash = h__2247__auto____17804;
    return h__2247__auto____17804
  }
};
cljs.core.List.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17805 = this;
  if(this__17805.count === 1) {
    return null
  }else {
    return this__17805.rest
  }
};
cljs.core.List.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17806 = this;
  return new cljs.core.List(this__17806.meta, o, coll, this__17806.count + 1, null)
};
cljs.core.List.prototype.toString = function() {
  var this__17807 = this;
  var this__17808 = this;
  return cljs.core.pr_str.call(null, this__17808)
};
cljs.core.List.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17809 = this;
  return coll
};
cljs.core.List.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__17810 = this;
  return this__17810.count
};
cljs.core.List.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__17811 = this;
  return this__17811.first
};
cljs.core.List.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__17812 = this;
  return coll.cljs$core$ISeq$_rest$arity$1(coll)
};
cljs.core.List.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17813 = this;
  return this__17813.first
};
cljs.core.List.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17814 = this;
  if(this__17814.count === 1) {
    return cljs.core.List.EMPTY
  }else {
    return this__17814.rest
  }
};
cljs.core.List.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17815 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.List.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17816 = this;
  return new cljs.core.List(meta, this__17816.first, this__17816.rest, this__17816.count, this__17816.__hash)
};
cljs.core.List.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17817 = this;
  return this__17817.meta
};
cljs.core.List.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17818 = this;
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
  var this__17819 = this;
  return 0
};
cljs.core.EmptyList.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17820 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17821 = this;
  return new cljs.core.List(this__17821.meta, o, null, 1, null)
};
cljs.core.EmptyList.prototype.toString = function() {
  var this__17822 = this;
  var this__17823 = this;
  return cljs.core.pr_str.call(null, this__17823)
};
cljs.core.EmptyList.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17824 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__17825 = this;
  return 0
};
cljs.core.EmptyList.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__17826 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__17827 = this;
  throw new Error("Can't pop empty list");
};
cljs.core.EmptyList.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17828 = this;
  return null
};
cljs.core.EmptyList.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17829 = this;
  return cljs.core.List.EMPTY
};
cljs.core.EmptyList.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17830 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.EmptyList.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17831 = this;
  return new cljs.core.EmptyList(meta)
};
cljs.core.EmptyList.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17832 = this;
  return this__17832.meta
};
cljs.core.EmptyList.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17833 = this;
  return coll
};
cljs.core.EmptyList;
cljs.core.List.EMPTY = new cljs.core.EmptyList(null);
cljs.core.reversible_QMARK_ = function reversible_QMARK_(coll) {
  var G__17837__17838 = coll;
  if(G__17837__17838) {
    if(function() {
      var or__3824__auto____17839 = G__17837__17838.cljs$lang$protocol_mask$partition0$ & 134217728;
      if(or__3824__auto____17839) {
        return or__3824__auto____17839
      }else {
        return G__17837__17838.cljs$core$IReversible$
      }
    }()) {
      return true
    }else {
      if(!G__17837__17838.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IReversible, G__17837__17838)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IReversible, G__17837__17838)
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
    var G__17840__delegate = function(x, y, z, items) {
      return cljs.core.conj.call(null, cljs.core.conj.call(null, cljs.core.conj.call(null, cljs.core.reduce.call(null, cljs.core.conj, cljs.core.List.EMPTY, cljs.core.reverse.call(null, items)), z), y), x)
    };
    var G__17840 = function(x, y, z, var_args) {
      var items = null;
      if(goog.isDef(var_args)) {
        items = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__17840__delegate.call(this, x, y, z, items)
    };
    G__17840.cljs$lang$maxFixedArity = 3;
    G__17840.cljs$lang$applyTo = function(arglist__17841) {
      var x = cljs.core.first(arglist__17841);
      var y = cljs.core.first(cljs.core.next(arglist__17841));
      var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__17841)));
      var items = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__17841)));
      return G__17840__delegate(x, y, z, items)
    };
    G__17840.cljs$lang$arity$variadic = G__17840__delegate;
    return G__17840
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
  var this__17842 = this;
  var h__2247__auto____17843 = this__17842.__hash;
  if(!(h__2247__auto____17843 == null)) {
    return h__2247__auto____17843
  }else {
    var h__2247__auto____17844 = cljs.core.hash_coll.call(null, coll);
    this__17842.__hash = h__2247__auto____17844;
    return h__2247__auto____17844
  }
};
cljs.core.Cons.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17845 = this;
  if(this__17845.rest == null) {
    return null
  }else {
    return cljs.core._seq.call(null, this__17845.rest)
  }
};
cljs.core.Cons.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17846 = this;
  return new cljs.core.Cons(null, o, coll, this__17846.__hash)
};
cljs.core.Cons.prototype.toString = function() {
  var this__17847 = this;
  var this__17848 = this;
  return cljs.core.pr_str.call(null, this__17848)
};
cljs.core.Cons.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17849 = this;
  return coll
};
cljs.core.Cons.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17850 = this;
  return this__17850.first
};
cljs.core.Cons.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17851 = this;
  if(this__17851.rest == null) {
    return cljs.core.List.EMPTY
  }else {
    return this__17851.rest
  }
};
cljs.core.Cons.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17852 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.Cons.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17853 = this;
  return new cljs.core.Cons(meta, this__17853.first, this__17853.rest, this__17853.__hash)
};
cljs.core.Cons.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17854 = this;
  return this__17854.meta
};
cljs.core.Cons.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17855 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17855.meta)
};
cljs.core.Cons;
cljs.core.cons = function cons(x, coll) {
  if(function() {
    var or__3824__auto____17860 = coll == null;
    if(or__3824__auto____17860) {
      return or__3824__auto____17860
    }else {
      var G__17861__17862 = coll;
      if(G__17861__17862) {
        if(function() {
          var or__3824__auto____17863 = G__17861__17862.cljs$lang$protocol_mask$partition0$ & 64;
          if(or__3824__auto____17863) {
            return or__3824__auto____17863
          }else {
            return G__17861__17862.cljs$core$ISeq$
          }
        }()) {
          return true
        }else {
          if(!G__17861__17862.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17861__17862)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.ISeq, G__17861__17862)
      }
    }
  }()) {
    return new cljs.core.Cons(null, x, coll, null)
  }else {
    return new cljs.core.Cons(null, x, cljs.core.seq.call(null, coll), null)
  }
};
cljs.core.list_QMARK_ = function list_QMARK_(x) {
  var G__17867__17868 = x;
  if(G__17867__17868) {
    if(function() {
      var or__3824__auto____17869 = G__17867__17868.cljs$lang$protocol_mask$partition0$ & 33554432;
      if(or__3824__auto____17869) {
        return or__3824__auto____17869
      }else {
        return G__17867__17868.cljs$core$IList$
      }
    }()) {
      return true
    }else {
      if(!G__17867__17868.cljs$lang$protocol_mask$partition0$) {
        return cljs.core.type_satisfies_.call(null, cljs.core.IList, G__17867__17868)
      }else {
        return false
      }
    }
  }else {
    return cljs.core.type_satisfies_.call(null, cljs.core.IList, G__17867__17868)
  }
};
cljs.core.IReduce["string"] = true;
cljs.core._reduce["string"] = function() {
  var G__17870 = null;
  var G__17870__2 = function(string, f) {
    return cljs.core.ci_reduce.call(null, string, f)
  };
  var G__17870__3 = function(string, f, start) {
    return cljs.core.ci_reduce.call(null, string, f, start)
  };
  G__17870 = function(string, f, start) {
    switch(arguments.length) {
      case 2:
        return G__17870__2.call(this, string, f);
      case 3:
        return G__17870__3.call(this, string, f, start)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17870
}();
cljs.core.ILookup["string"] = true;
cljs.core._lookup["string"] = function() {
  var G__17871 = null;
  var G__17871__2 = function(string, k) {
    return cljs.core._nth.call(null, string, k)
  };
  var G__17871__3 = function(string, k, not_found) {
    return cljs.core._nth.call(null, string, k, not_found)
  };
  G__17871 = function(string, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17871__2.call(this, string, k);
      case 3:
        return G__17871__3.call(this, string, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17871
}();
cljs.core.IIndexed["string"] = true;
cljs.core._nth["string"] = function() {
  var G__17872 = null;
  var G__17872__2 = function(string, n) {
    if(n < cljs.core._count.call(null, string)) {
      return string.charAt(n)
    }else {
      return null
    }
  };
  var G__17872__3 = function(string, n, not_found) {
    if(n < cljs.core._count.call(null, string)) {
      return string.charAt(n)
    }else {
      return not_found
    }
  };
  G__17872 = function(string, n, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17872__2.call(this, string, n);
      case 3:
        return G__17872__3.call(this, string, n, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17872
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
  var G__17884 = null;
  var G__17884__2 = function(this_sym17875, coll) {
    var this__17877 = this;
    var this_sym17875__17878 = this;
    var ___17879 = this_sym17875__17878;
    if(coll == null) {
      return null
    }else {
      var strobj__17880 = coll.strobj;
      if(strobj__17880 == null) {
        return cljs.core._lookup.call(null, coll, this__17877.k, null)
      }else {
        return strobj__17880[this__17877.k]
      }
    }
  };
  var G__17884__3 = function(this_sym17876, coll, not_found) {
    var this__17877 = this;
    var this_sym17876__17881 = this;
    var ___17882 = this_sym17876__17881;
    if(coll == null) {
      return not_found
    }else {
      return cljs.core._lookup.call(null, coll, this__17877.k, not_found)
    }
  };
  G__17884 = function(this_sym17876, coll, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17884__2.call(this, this_sym17876, coll);
      case 3:
        return G__17884__3.call(this, this_sym17876, coll, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17884
}();
cljs.core.Keyword.prototype.apply = function(this_sym17873, args17874) {
  var this__17883 = this;
  return this_sym17873.call.apply(this_sym17873, [this_sym17873].concat(args17874.slice()))
};
cljs.core.Keyword;
String.prototype.cljs$core$IFn$ = true;
String.prototype.call = function() {
  var G__17893 = null;
  var G__17893__2 = function(this_sym17887, coll) {
    var this_sym17887__17889 = this;
    var this__17890 = this_sym17887__17889;
    return cljs.core._lookup.call(null, coll, this__17890.toString(), null)
  };
  var G__17893__3 = function(this_sym17888, coll, not_found) {
    var this_sym17888__17891 = this;
    var this__17892 = this_sym17888__17891;
    return cljs.core._lookup.call(null, coll, this__17892.toString(), not_found)
  };
  G__17893 = function(this_sym17888, coll, not_found) {
    switch(arguments.length) {
      case 2:
        return G__17893__2.call(this, this_sym17888, coll);
      case 3:
        return G__17893__3.call(this, this_sym17888, coll, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__17893
}();
String.prototype.apply = function(this_sym17885, args17886) {
  return this_sym17885.call.apply(this_sym17885, [this_sym17885].concat(args17886.slice()))
};
String.prototype.apply = function(s, args) {
  if(cljs.core.count.call(null, args) < 2) {
    return cljs.core._lookup.call(null, args[0], s, null)
  }else {
    return cljs.core._lookup.call(null, args[0], s, args[1])
  }
};
cljs.core.lazy_seq_value = function lazy_seq_value(lazy_seq) {
  var x__17895 = lazy_seq.x;
  if(lazy_seq.realized) {
    return x__17895
  }else {
    lazy_seq.x = x__17895.call(null);
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
  var this__17896 = this;
  var h__2247__auto____17897 = this__17896.__hash;
  if(!(h__2247__auto____17897 == null)) {
    return h__2247__auto____17897
  }else {
    var h__2247__auto____17898 = cljs.core.hash_coll.call(null, coll);
    this__17896.__hash = h__2247__auto____17898;
    return h__2247__auto____17898
  }
};
cljs.core.LazySeq.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__17899 = this;
  return cljs.core._seq.call(null, coll.cljs$core$ISeq$_rest$arity$1(coll))
};
cljs.core.LazySeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__17900 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.LazySeq.prototype.toString = function() {
  var this__17901 = this;
  var this__17902 = this;
  return cljs.core.pr_str.call(null, this__17902)
};
cljs.core.LazySeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17903 = this;
  return cljs.core.seq.call(null, cljs.core.lazy_seq_value.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17904 = this;
  return cljs.core.first.call(null, cljs.core.lazy_seq_value.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17905 = this;
  return cljs.core.rest.call(null, cljs.core.lazy_seq_value.call(null, coll))
};
cljs.core.LazySeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17906 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.LazySeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__17907 = this;
  return new cljs.core.LazySeq(meta, this__17907.realized, this__17907.x, this__17907.__hash)
};
cljs.core.LazySeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17908 = this;
  return this__17908.meta
};
cljs.core.LazySeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17909 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17909.meta)
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
  var this__17910 = this;
  return this__17910.end
};
cljs.core.ChunkBuffer.prototype.add = function(o) {
  var this__17911 = this;
  var ___17912 = this;
  this__17911.buf[this__17911.end] = o;
  return this__17911.end = this__17911.end + 1
};
cljs.core.ChunkBuffer.prototype.chunk = function(o) {
  var this__17913 = this;
  var ___17914 = this;
  var ret__17915 = new cljs.core.ArrayChunk(this__17913.buf, 0, this__17913.end);
  this__17913.buf = null;
  return ret__17915
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
  var this__17916 = this;
  return cljs.core.array_reduce.call(null, this__17916.arr, f, this__17916.arr[this__17916.off], this__17916.off + 1)
};
cljs.core.ArrayChunk.prototype.cljs$core$IReduce$_reduce$arity$3 = function(coll, f, start) {
  var this__17917 = this;
  return cljs.core.array_reduce.call(null, this__17917.arr, f, start, this__17917.off)
};
cljs.core.ArrayChunk.prototype.cljs$core$IChunk$ = true;
cljs.core.ArrayChunk.prototype.cljs$core$IChunk$_drop_first$arity$1 = function(coll) {
  var this__17918 = this;
  if(this__17918.off === this__17918.end) {
    throw new Error("-drop-first of empty chunk");
  }else {
    return new cljs.core.ArrayChunk(this__17918.arr, this__17918.off + 1, this__17918.end)
  }
};
cljs.core.ArrayChunk.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, i) {
  var this__17919 = this;
  return this__17919.arr[this__17919.off + i]
};
cljs.core.ArrayChunk.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, i, not_found) {
  var this__17920 = this;
  if(function() {
    var and__3822__auto____17921 = i >= 0;
    if(and__3822__auto____17921) {
      return i < this__17920.end - this__17920.off
    }else {
      return and__3822__auto____17921
    }
  }()) {
    return this__17920.arr[this__17920.off + i]
  }else {
    return not_found
  }
};
cljs.core.ArrayChunk.prototype.cljs$core$ICounted$_count$arity$1 = function(_) {
  var this__17922 = this;
  return this__17922.end - this__17922.off
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
  var this__17923 = this;
  var h__2247__auto____17924 = this__17923.__hash;
  if(!(h__2247__auto____17924 == null)) {
    return h__2247__auto____17924
  }else {
    var h__2247__auto____17925 = cljs.core.hash_coll.call(null, coll);
    this__17923.__hash = h__2247__auto____17925;
    return h__2247__auto____17925
  }
};
cljs.core.ChunkedCons.prototype.cljs$core$ICollection$_conj$arity$2 = function(this$, o) {
  var this__17926 = this;
  return cljs.core.cons.call(null, o, this$)
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__17927 = this;
  return coll
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__17928 = this;
  return cljs.core._nth.call(null, this__17928.chunk, 0)
};
cljs.core.ChunkedCons.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__17929 = this;
  if(cljs.core._count.call(null, this__17929.chunk) > 1) {
    return new cljs.core.ChunkedCons(cljs.core._drop_first.call(null, this__17929.chunk), this__17929.more, this__17929.meta, null)
  }else {
    if(this__17929.more == null) {
      return cljs.core.List.EMPTY
    }else {
      return this__17929.more
    }
  }
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedNext$_chunked_next$arity$1 = function(coll) {
  var this__17930 = this;
  if(this__17930.more == null) {
    return null
  }else {
    return this__17930.more
  }
};
cljs.core.ChunkedCons.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__17931 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.ChunkedCons.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, m) {
  var this__17932 = this;
  return new cljs.core.ChunkedCons(this__17932.chunk, this__17932.more, m, this__17932.__hash)
};
cljs.core.ChunkedCons.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__17933 = this;
  return this__17933.meta
};
cljs.core.ChunkedCons.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__17934 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__17934.meta)
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedSeq$_chunked_first$arity$1 = function(coll) {
  var this__17935 = this;
  return this__17935.chunk
};
cljs.core.ChunkedCons.prototype.cljs$core$IChunkedSeq$_chunked_rest$arity$1 = function(coll) {
  var this__17936 = this;
  if(this__17936.more == null) {
    return cljs.core.List.EMPTY
  }else {
    return this__17936.more
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
    var G__17940__17941 = s;
    if(G__17940__17941) {
      if(function() {
        var or__3824__auto____17942 = G__17940__17941.cljs$lang$protocol_mask$partition1$ & 1024;
        if(or__3824__auto____17942) {
          return or__3824__auto____17942
        }else {
          return G__17940__17941.cljs$core$IChunkedNext$
        }
      }()) {
        return true
      }else {
        if(!G__17940__17941.cljs$lang$protocol_mask$partition1$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedNext, G__17940__17941)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IChunkedNext, G__17940__17941)
    }
  }()) {
    return cljs.core._chunked_next.call(null, s)
  }else {
    return cljs.core.seq.call(null, cljs.core._chunked_rest.call(null, s))
  }
};
cljs.core.to_array = function to_array(s) {
  var ary__17945 = [];
  var s__17946 = s;
  while(true) {
    if(cljs.core.seq.call(null, s__17946)) {
      ary__17945.push(cljs.core.first.call(null, s__17946));
      var G__17947 = cljs.core.next.call(null, s__17946);
      s__17946 = G__17947;
      continue
    }else {
      return ary__17945
    }
    break
  }
};
cljs.core.to_array_2d = function to_array_2d(coll) {
  var ret__17951 = cljs.core.make_array.call(null, cljs.core.count.call(null, coll));
  var i__17952 = 0;
  var xs__17953 = cljs.core.seq.call(null, coll);
  while(true) {
    if(xs__17953) {
      ret__17951[i__17952] = cljs.core.to_array.call(null, cljs.core.first.call(null, xs__17953));
      var G__17954 = i__17952 + 1;
      var G__17955 = cljs.core.next.call(null, xs__17953);
      i__17952 = G__17954;
      xs__17953 = G__17955;
      continue
    }else {
    }
    break
  }
  return ret__17951
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
    var a__17963 = cljs.core.make_array.call(null, size);
    if(cljs.core.seq_QMARK_.call(null, init_val_or_seq)) {
      var s__17964 = cljs.core.seq.call(null, init_val_or_seq);
      var i__17965 = 0;
      var s__17966 = s__17964;
      while(true) {
        if(cljs.core.truth_(function() {
          var and__3822__auto____17967 = s__17966;
          if(and__3822__auto____17967) {
            return i__17965 < size
          }else {
            return and__3822__auto____17967
          }
        }())) {
          a__17963[i__17965] = cljs.core.first.call(null, s__17966);
          var G__17970 = i__17965 + 1;
          var G__17971 = cljs.core.next.call(null, s__17966);
          i__17965 = G__17970;
          s__17966 = G__17971;
          continue
        }else {
          return a__17963
        }
        break
      }
    }else {
      var n__2593__auto____17968 = size;
      var i__17969 = 0;
      while(true) {
        if(i__17969 < n__2593__auto____17968) {
          a__17963[i__17969] = init_val_or_seq;
          var G__17972 = i__17969 + 1;
          i__17969 = G__17972;
          continue
        }else {
        }
        break
      }
      return a__17963
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
    var a__17980 = cljs.core.make_array.call(null, size);
    if(cljs.core.seq_QMARK_.call(null, init_val_or_seq)) {
      var s__17981 = cljs.core.seq.call(null, init_val_or_seq);
      var i__17982 = 0;
      var s__17983 = s__17981;
      while(true) {
        if(cljs.core.truth_(function() {
          var and__3822__auto____17984 = s__17983;
          if(and__3822__auto____17984) {
            return i__17982 < size
          }else {
            return and__3822__auto____17984
          }
        }())) {
          a__17980[i__17982] = cljs.core.first.call(null, s__17983);
          var G__17987 = i__17982 + 1;
          var G__17988 = cljs.core.next.call(null, s__17983);
          i__17982 = G__17987;
          s__17983 = G__17988;
          continue
        }else {
          return a__17980
        }
        break
      }
    }else {
      var n__2593__auto____17985 = size;
      var i__17986 = 0;
      while(true) {
        if(i__17986 < n__2593__auto____17985) {
          a__17980[i__17986] = init_val_or_seq;
          var G__17989 = i__17986 + 1;
          i__17986 = G__17989;
          continue
        }else {
        }
        break
      }
      return a__17980
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
    var a__17997 = cljs.core.make_array.call(null, size);
    if(cljs.core.seq_QMARK_.call(null, init_val_or_seq)) {
      var s__17998 = cljs.core.seq.call(null, init_val_or_seq);
      var i__17999 = 0;
      var s__18000 = s__17998;
      while(true) {
        if(cljs.core.truth_(function() {
          var and__3822__auto____18001 = s__18000;
          if(and__3822__auto____18001) {
            return i__17999 < size
          }else {
            return and__3822__auto____18001
          }
        }())) {
          a__17997[i__17999] = cljs.core.first.call(null, s__18000);
          var G__18004 = i__17999 + 1;
          var G__18005 = cljs.core.next.call(null, s__18000);
          i__17999 = G__18004;
          s__18000 = G__18005;
          continue
        }else {
          return a__17997
        }
        break
      }
    }else {
      var n__2593__auto____18002 = size;
      var i__18003 = 0;
      while(true) {
        if(i__18003 < n__2593__auto____18002) {
          a__17997[i__18003] = init_val_or_seq;
          var G__18006 = i__18003 + 1;
          i__18003 = G__18006;
          continue
        }else {
        }
        break
      }
      return a__17997
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
    var s__18011 = s;
    var i__18012 = n;
    var sum__18013 = 0;
    while(true) {
      if(cljs.core.truth_(function() {
        var and__3822__auto____18014 = i__18012 > 0;
        if(and__3822__auto____18014) {
          return cljs.core.seq.call(null, s__18011)
        }else {
          return and__3822__auto____18014
        }
      }())) {
        var G__18015 = cljs.core.next.call(null, s__18011);
        var G__18016 = i__18012 - 1;
        var G__18017 = sum__18013 + 1;
        s__18011 = G__18015;
        i__18012 = G__18016;
        sum__18013 = G__18017;
        continue
      }else {
        return sum__18013
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
      var s__18022 = cljs.core.seq.call(null, x);
      if(s__18022) {
        if(cljs.core.chunked_seq_QMARK_.call(null, s__18022)) {
          return cljs.core.chunk_cons.call(null, cljs.core.chunk_first.call(null, s__18022), concat.call(null, cljs.core.chunk_rest.call(null, s__18022), y))
        }else {
          return cljs.core.cons.call(null, cljs.core.first.call(null, s__18022), concat.call(null, cljs.core.rest.call(null, s__18022), y))
        }
      }else {
        return y
      }
    }, null)
  };
  var concat__3 = function() {
    var G__18026__delegate = function(x, y, zs) {
      var cat__18025 = function cat(xys, zs) {
        return new cljs.core.LazySeq(null, false, function() {
          var xys__18024 = cljs.core.seq.call(null, xys);
          if(xys__18024) {
            if(cljs.core.chunked_seq_QMARK_.call(null, xys__18024)) {
              return cljs.core.chunk_cons.call(null, cljs.core.chunk_first.call(null, xys__18024), cat.call(null, cljs.core.chunk_rest.call(null, xys__18024), zs))
            }else {
              return cljs.core.cons.call(null, cljs.core.first.call(null, xys__18024), cat.call(null, cljs.core.rest.call(null, xys__18024), zs))
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
      return cat__18025.call(null, concat.call(null, x, y), zs)
    };
    var G__18026 = function(x, y, var_args) {
      var zs = null;
      if(goog.isDef(var_args)) {
        zs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__18026__delegate.call(this, x, y, zs)
    };
    G__18026.cljs$lang$maxFixedArity = 2;
    G__18026.cljs$lang$applyTo = function(arglist__18027) {
      var x = cljs.core.first(arglist__18027);
      var y = cljs.core.first(cljs.core.next(arglist__18027));
      var zs = cljs.core.rest(cljs.core.next(arglist__18027));
      return G__18026__delegate(x, y, zs)
    };
    G__18026.cljs$lang$arity$variadic = G__18026__delegate;
    return G__18026
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
    var G__18028__delegate = function(a, b, c, d, more) {
      return cljs.core.cons.call(null, a, cljs.core.cons.call(null, b, cljs.core.cons.call(null, c, cljs.core.cons.call(null, d, cljs.core.spread.call(null, more)))))
    };
    var G__18028 = function(a, b, c, d, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__18028__delegate.call(this, a, b, c, d, more)
    };
    G__18028.cljs$lang$maxFixedArity = 4;
    G__18028.cljs$lang$applyTo = function(arglist__18029) {
      var a = cljs.core.first(arglist__18029);
      var b = cljs.core.first(cljs.core.next(arglist__18029));
      var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18029)));
      var d = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18029))));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18029))));
      return G__18028__delegate(a, b, c, d, more)
    };
    G__18028.cljs$lang$arity$variadic = G__18028__delegate;
    return G__18028
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
  var args__18071 = cljs.core.seq.call(null, args);
  if(argc === 0) {
    return f.call(null)
  }else {
    var a__18072 = cljs.core._first.call(null, args__18071);
    var args__18073 = cljs.core._rest.call(null, args__18071);
    if(argc === 1) {
      if(f.cljs$lang$arity$1) {
        return f.cljs$lang$arity$1(a__18072)
      }else {
        return f.call(null, a__18072)
      }
    }else {
      var b__18074 = cljs.core._first.call(null, args__18073);
      var args__18075 = cljs.core._rest.call(null, args__18073);
      if(argc === 2) {
        if(f.cljs$lang$arity$2) {
          return f.cljs$lang$arity$2(a__18072, b__18074)
        }else {
          return f.call(null, a__18072, b__18074)
        }
      }else {
        var c__18076 = cljs.core._first.call(null, args__18075);
        var args__18077 = cljs.core._rest.call(null, args__18075);
        if(argc === 3) {
          if(f.cljs$lang$arity$3) {
            return f.cljs$lang$arity$3(a__18072, b__18074, c__18076)
          }else {
            return f.call(null, a__18072, b__18074, c__18076)
          }
        }else {
          var d__18078 = cljs.core._first.call(null, args__18077);
          var args__18079 = cljs.core._rest.call(null, args__18077);
          if(argc === 4) {
            if(f.cljs$lang$arity$4) {
              return f.cljs$lang$arity$4(a__18072, b__18074, c__18076, d__18078)
            }else {
              return f.call(null, a__18072, b__18074, c__18076, d__18078)
            }
          }else {
            var e__18080 = cljs.core._first.call(null, args__18079);
            var args__18081 = cljs.core._rest.call(null, args__18079);
            if(argc === 5) {
              if(f.cljs$lang$arity$5) {
                return f.cljs$lang$arity$5(a__18072, b__18074, c__18076, d__18078, e__18080)
              }else {
                return f.call(null, a__18072, b__18074, c__18076, d__18078, e__18080)
              }
            }else {
              var f__18082 = cljs.core._first.call(null, args__18081);
              var args__18083 = cljs.core._rest.call(null, args__18081);
              if(argc === 6) {
                if(f__18082.cljs$lang$arity$6) {
                  return f__18082.cljs$lang$arity$6(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082)
                }else {
                  return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082)
                }
              }else {
                var g__18084 = cljs.core._first.call(null, args__18083);
                var args__18085 = cljs.core._rest.call(null, args__18083);
                if(argc === 7) {
                  if(f__18082.cljs$lang$arity$7) {
                    return f__18082.cljs$lang$arity$7(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084)
                  }else {
                    return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084)
                  }
                }else {
                  var h__18086 = cljs.core._first.call(null, args__18085);
                  var args__18087 = cljs.core._rest.call(null, args__18085);
                  if(argc === 8) {
                    if(f__18082.cljs$lang$arity$8) {
                      return f__18082.cljs$lang$arity$8(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086)
                    }else {
                      return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086)
                    }
                  }else {
                    var i__18088 = cljs.core._first.call(null, args__18087);
                    var args__18089 = cljs.core._rest.call(null, args__18087);
                    if(argc === 9) {
                      if(f__18082.cljs$lang$arity$9) {
                        return f__18082.cljs$lang$arity$9(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088)
                      }else {
                        return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088)
                      }
                    }else {
                      var j__18090 = cljs.core._first.call(null, args__18089);
                      var args__18091 = cljs.core._rest.call(null, args__18089);
                      if(argc === 10) {
                        if(f__18082.cljs$lang$arity$10) {
                          return f__18082.cljs$lang$arity$10(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090)
                        }else {
                          return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090)
                        }
                      }else {
                        var k__18092 = cljs.core._first.call(null, args__18091);
                        var args__18093 = cljs.core._rest.call(null, args__18091);
                        if(argc === 11) {
                          if(f__18082.cljs$lang$arity$11) {
                            return f__18082.cljs$lang$arity$11(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092)
                          }else {
                            return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092)
                          }
                        }else {
                          var l__18094 = cljs.core._first.call(null, args__18093);
                          var args__18095 = cljs.core._rest.call(null, args__18093);
                          if(argc === 12) {
                            if(f__18082.cljs$lang$arity$12) {
                              return f__18082.cljs$lang$arity$12(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094)
                            }else {
                              return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094)
                            }
                          }else {
                            var m__18096 = cljs.core._first.call(null, args__18095);
                            var args__18097 = cljs.core._rest.call(null, args__18095);
                            if(argc === 13) {
                              if(f__18082.cljs$lang$arity$13) {
                                return f__18082.cljs$lang$arity$13(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096)
                              }else {
                                return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096)
                              }
                            }else {
                              var n__18098 = cljs.core._first.call(null, args__18097);
                              var args__18099 = cljs.core._rest.call(null, args__18097);
                              if(argc === 14) {
                                if(f__18082.cljs$lang$arity$14) {
                                  return f__18082.cljs$lang$arity$14(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098)
                                }else {
                                  return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098)
                                }
                              }else {
                                var o__18100 = cljs.core._first.call(null, args__18099);
                                var args__18101 = cljs.core._rest.call(null, args__18099);
                                if(argc === 15) {
                                  if(f__18082.cljs$lang$arity$15) {
                                    return f__18082.cljs$lang$arity$15(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100)
                                  }else {
                                    return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100)
                                  }
                                }else {
                                  var p__18102 = cljs.core._first.call(null, args__18101);
                                  var args__18103 = cljs.core._rest.call(null, args__18101);
                                  if(argc === 16) {
                                    if(f__18082.cljs$lang$arity$16) {
                                      return f__18082.cljs$lang$arity$16(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102)
                                    }else {
                                      return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102)
                                    }
                                  }else {
                                    var q__18104 = cljs.core._first.call(null, args__18103);
                                    var args__18105 = cljs.core._rest.call(null, args__18103);
                                    if(argc === 17) {
                                      if(f__18082.cljs$lang$arity$17) {
                                        return f__18082.cljs$lang$arity$17(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104)
                                      }else {
                                        return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104)
                                      }
                                    }else {
                                      var r__18106 = cljs.core._first.call(null, args__18105);
                                      var args__18107 = cljs.core._rest.call(null, args__18105);
                                      if(argc === 18) {
                                        if(f__18082.cljs$lang$arity$18) {
                                          return f__18082.cljs$lang$arity$18(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104, r__18106)
                                        }else {
                                          return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104, r__18106)
                                        }
                                      }else {
                                        var s__18108 = cljs.core._first.call(null, args__18107);
                                        var args__18109 = cljs.core._rest.call(null, args__18107);
                                        if(argc === 19) {
                                          if(f__18082.cljs$lang$arity$19) {
                                            return f__18082.cljs$lang$arity$19(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104, r__18106, s__18108)
                                          }else {
                                            return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104, r__18106, s__18108)
                                          }
                                        }else {
                                          var t__18110 = cljs.core._first.call(null, args__18109);
                                          var args__18111 = cljs.core._rest.call(null, args__18109);
                                          if(argc === 20) {
                                            if(f__18082.cljs$lang$arity$20) {
                                              return f__18082.cljs$lang$arity$20(a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104, r__18106, s__18108, t__18110)
                                            }else {
                                              return f__18082.call(null, a__18072, b__18074, c__18076, d__18078, e__18080, f__18082, g__18084, h__18086, i__18088, j__18090, k__18092, l__18094, m__18096, n__18098, o__18100, p__18102, q__18104, r__18106, s__18108, t__18110)
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
    var fixed_arity__18126 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__18127 = cljs.core.bounded_count.call(null, args, fixed_arity__18126 + 1);
      if(bc__18127 <= fixed_arity__18126) {
        return cljs.core.apply_to.call(null, f, bc__18127, args)
      }else {
        return f.cljs$lang$applyTo(args)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, args))
    }
  };
  var apply__3 = function(f, x, args) {
    var arglist__18128 = cljs.core.list_STAR_.call(null, x, args);
    var fixed_arity__18129 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__18130 = cljs.core.bounded_count.call(null, arglist__18128, fixed_arity__18129 + 1);
      if(bc__18130 <= fixed_arity__18129) {
        return cljs.core.apply_to.call(null, f, bc__18130, arglist__18128)
      }else {
        return f.cljs$lang$applyTo(arglist__18128)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, arglist__18128))
    }
  };
  var apply__4 = function(f, x, y, args) {
    var arglist__18131 = cljs.core.list_STAR_.call(null, x, y, args);
    var fixed_arity__18132 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__18133 = cljs.core.bounded_count.call(null, arglist__18131, fixed_arity__18132 + 1);
      if(bc__18133 <= fixed_arity__18132) {
        return cljs.core.apply_to.call(null, f, bc__18133, arglist__18131)
      }else {
        return f.cljs$lang$applyTo(arglist__18131)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, arglist__18131))
    }
  };
  var apply__5 = function(f, x, y, z, args) {
    var arglist__18134 = cljs.core.list_STAR_.call(null, x, y, z, args);
    var fixed_arity__18135 = f.cljs$lang$maxFixedArity;
    if(f.cljs$lang$applyTo) {
      var bc__18136 = cljs.core.bounded_count.call(null, arglist__18134, fixed_arity__18135 + 1);
      if(bc__18136 <= fixed_arity__18135) {
        return cljs.core.apply_to.call(null, f, bc__18136, arglist__18134)
      }else {
        return f.cljs$lang$applyTo(arglist__18134)
      }
    }else {
      return f.apply(f, cljs.core.to_array.call(null, arglist__18134))
    }
  };
  var apply__6 = function() {
    var G__18140__delegate = function(f, a, b, c, d, args) {
      var arglist__18137 = cljs.core.cons.call(null, a, cljs.core.cons.call(null, b, cljs.core.cons.call(null, c, cljs.core.cons.call(null, d, cljs.core.spread.call(null, args)))));
      var fixed_arity__18138 = f.cljs$lang$maxFixedArity;
      if(f.cljs$lang$applyTo) {
        var bc__18139 = cljs.core.bounded_count.call(null, arglist__18137, fixed_arity__18138 + 1);
        if(bc__18139 <= fixed_arity__18138) {
          return cljs.core.apply_to.call(null, f, bc__18139, arglist__18137)
        }else {
          return f.cljs$lang$applyTo(arglist__18137)
        }
      }else {
        return f.apply(f, cljs.core.to_array.call(null, arglist__18137))
      }
    };
    var G__18140 = function(f, a, b, c, d, var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 5), 0)
      }
      return G__18140__delegate.call(this, f, a, b, c, d, args)
    };
    G__18140.cljs$lang$maxFixedArity = 5;
    G__18140.cljs$lang$applyTo = function(arglist__18141) {
      var f = cljs.core.first(arglist__18141);
      var a = cljs.core.first(cljs.core.next(arglist__18141));
      var b = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18141)));
      var c = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18141))));
      var d = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18141)))));
      var args = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18141)))));
      return G__18140__delegate(f, a, b, c, d, args)
    };
    G__18140.cljs$lang$arity$variadic = G__18140__delegate;
    return G__18140
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
  vary_meta.cljs$lang$applyTo = function(arglist__18142) {
    var obj = cljs.core.first(arglist__18142);
    var f = cljs.core.first(cljs.core.next(arglist__18142));
    var args = cljs.core.rest(cljs.core.next(arglist__18142));
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
    var G__18143__delegate = function(x, y, more) {
      return cljs.core.not.call(null, cljs.core.apply.call(null, cljs.core._EQ_, x, y, more))
    };
    var G__18143 = function(x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__18143__delegate.call(this, x, y, more)
    };
    G__18143.cljs$lang$maxFixedArity = 2;
    G__18143.cljs$lang$applyTo = function(arglist__18144) {
      var x = cljs.core.first(arglist__18144);
      var y = cljs.core.first(cljs.core.next(arglist__18144));
      var more = cljs.core.rest(cljs.core.next(arglist__18144));
      return G__18143__delegate(x, y, more)
    };
    G__18143.cljs$lang$arity$variadic = G__18143__delegate;
    return G__18143
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
        var G__18145 = pred;
        var G__18146 = cljs.core.next.call(null, coll);
        pred = G__18145;
        coll = G__18146;
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
      var or__3824__auto____18148 = pred.call(null, cljs.core.first.call(null, coll));
      if(cljs.core.truth_(or__3824__auto____18148)) {
        return or__3824__auto____18148
      }else {
        var G__18149 = pred;
        var G__18150 = cljs.core.next.call(null, coll);
        pred = G__18149;
        coll = G__18150;
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
    var G__18151 = null;
    var G__18151__0 = function() {
      return cljs.core.not.call(null, f.call(null))
    };
    var G__18151__1 = function(x) {
      return cljs.core.not.call(null, f.call(null, x))
    };
    var G__18151__2 = function(x, y) {
      return cljs.core.not.call(null, f.call(null, x, y))
    };
    var G__18151__3 = function() {
      var G__18152__delegate = function(x, y, zs) {
        return cljs.core.not.call(null, cljs.core.apply.call(null, f, x, y, zs))
      };
      var G__18152 = function(x, y, var_args) {
        var zs = null;
        if(goog.isDef(var_args)) {
          zs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
        }
        return G__18152__delegate.call(this, x, y, zs)
      };
      G__18152.cljs$lang$maxFixedArity = 2;
      G__18152.cljs$lang$applyTo = function(arglist__18153) {
        var x = cljs.core.first(arglist__18153);
        var y = cljs.core.first(cljs.core.next(arglist__18153));
        var zs = cljs.core.rest(cljs.core.next(arglist__18153));
        return G__18152__delegate(x, y, zs)
      };
      G__18152.cljs$lang$arity$variadic = G__18152__delegate;
      return G__18152
    }();
    G__18151 = function(x, y, var_args) {
      var zs = var_args;
      switch(arguments.length) {
        case 0:
          return G__18151__0.call(this);
        case 1:
          return G__18151__1.call(this, x);
        case 2:
          return G__18151__2.call(this, x, y);
        default:
          return G__18151__3.cljs$lang$arity$variadic(x, y, cljs.core.array_seq(arguments, 2))
      }
      throw"Invalid arity: " + arguments.length;
    };
    G__18151.cljs$lang$maxFixedArity = 2;
    G__18151.cljs$lang$applyTo = G__18151__3.cljs$lang$applyTo;
    return G__18151
  }()
};
cljs.core.constantly = function constantly(x) {
  return function() {
    var G__18154__delegate = function(args) {
      return x
    };
    var G__18154 = function(var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__18154__delegate.call(this, args)
    };
    G__18154.cljs$lang$maxFixedArity = 0;
    G__18154.cljs$lang$applyTo = function(arglist__18155) {
      var args = cljs.core.seq(arglist__18155);
      return G__18154__delegate(args)
    };
    G__18154.cljs$lang$arity$variadic = G__18154__delegate;
    return G__18154
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
      var G__18162 = null;
      var G__18162__0 = function() {
        return f.call(null, g.call(null))
      };
      var G__18162__1 = function(x) {
        return f.call(null, g.call(null, x))
      };
      var G__18162__2 = function(x, y) {
        return f.call(null, g.call(null, x, y))
      };
      var G__18162__3 = function(x, y, z) {
        return f.call(null, g.call(null, x, y, z))
      };
      var G__18162__4 = function() {
        var G__18163__delegate = function(x, y, z, args) {
          return f.call(null, cljs.core.apply.call(null, g, x, y, z, args))
        };
        var G__18163 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18163__delegate.call(this, x, y, z, args)
        };
        G__18163.cljs$lang$maxFixedArity = 3;
        G__18163.cljs$lang$applyTo = function(arglist__18164) {
          var x = cljs.core.first(arglist__18164);
          var y = cljs.core.first(cljs.core.next(arglist__18164));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18164)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18164)));
          return G__18163__delegate(x, y, z, args)
        };
        G__18163.cljs$lang$arity$variadic = G__18163__delegate;
        return G__18163
      }();
      G__18162 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__18162__0.call(this);
          case 1:
            return G__18162__1.call(this, x);
          case 2:
            return G__18162__2.call(this, x, y);
          case 3:
            return G__18162__3.call(this, x, y, z);
          default:
            return G__18162__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__18162.cljs$lang$maxFixedArity = 3;
      G__18162.cljs$lang$applyTo = G__18162__4.cljs$lang$applyTo;
      return G__18162
    }()
  };
  var comp__3 = function(f, g, h) {
    return function() {
      var G__18165 = null;
      var G__18165__0 = function() {
        return f.call(null, g.call(null, h.call(null)))
      };
      var G__18165__1 = function(x) {
        return f.call(null, g.call(null, h.call(null, x)))
      };
      var G__18165__2 = function(x, y) {
        return f.call(null, g.call(null, h.call(null, x, y)))
      };
      var G__18165__3 = function(x, y, z) {
        return f.call(null, g.call(null, h.call(null, x, y, z)))
      };
      var G__18165__4 = function() {
        var G__18166__delegate = function(x, y, z, args) {
          return f.call(null, g.call(null, cljs.core.apply.call(null, h, x, y, z, args)))
        };
        var G__18166 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18166__delegate.call(this, x, y, z, args)
        };
        G__18166.cljs$lang$maxFixedArity = 3;
        G__18166.cljs$lang$applyTo = function(arglist__18167) {
          var x = cljs.core.first(arglist__18167);
          var y = cljs.core.first(cljs.core.next(arglist__18167));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18167)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18167)));
          return G__18166__delegate(x, y, z, args)
        };
        G__18166.cljs$lang$arity$variadic = G__18166__delegate;
        return G__18166
      }();
      G__18165 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__18165__0.call(this);
          case 1:
            return G__18165__1.call(this, x);
          case 2:
            return G__18165__2.call(this, x, y);
          case 3:
            return G__18165__3.call(this, x, y, z);
          default:
            return G__18165__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__18165.cljs$lang$maxFixedArity = 3;
      G__18165.cljs$lang$applyTo = G__18165__4.cljs$lang$applyTo;
      return G__18165
    }()
  };
  var comp__4 = function() {
    var G__18168__delegate = function(f1, f2, f3, fs) {
      var fs__18159 = cljs.core.reverse.call(null, cljs.core.list_STAR_.call(null, f1, f2, f3, fs));
      return function() {
        var G__18169__delegate = function(args) {
          var ret__18160 = cljs.core.apply.call(null, cljs.core.first.call(null, fs__18159), args);
          var fs__18161 = cljs.core.next.call(null, fs__18159);
          while(true) {
            if(fs__18161) {
              var G__18170 = cljs.core.first.call(null, fs__18161).call(null, ret__18160);
              var G__18171 = cljs.core.next.call(null, fs__18161);
              ret__18160 = G__18170;
              fs__18161 = G__18171;
              continue
            }else {
              return ret__18160
            }
            break
          }
        };
        var G__18169 = function(var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
          }
          return G__18169__delegate.call(this, args)
        };
        G__18169.cljs$lang$maxFixedArity = 0;
        G__18169.cljs$lang$applyTo = function(arglist__18172) {
          var args = cljs.core.seq(arglist__18172);
          return G__18169__delegate(args)
        };
        G__18169.cljs$lang$arity$variadic = G__18169__delegate;
        return G__18169
      }()
    };
    var G__18168 = function(f1, f2, f3, var_args) {
      var fs = null;
      if(goog.isDef(var_args)) {
        fs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__18168__delegate.call(this, f1, f2, f3, fs)
    };
    G__18168.cljs$lang$maxFixedArity = 3;
    G__18168.cljs$lang$applyTo = function(arglist__18173) {
      var f1 = cljs.core.first(arglist__18173);
      var f2 = cljs.core.first(cljs.core.next(arglist__18173));
      var f3 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18173)));
      var fs = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18173)));
      return G__18168__delegate(f1, f2, f3, fs)
    };
    G__18168.cljs$lang$arity$variadic = G__18168__delegate;
    return G__18168
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
      var G__18174__delegate = function(args) {
        return cljs.core.apply.call(null, f, arg1, args)
      };
      var G__18174 = function(var_args) {
        var args = null;
        if(goog.isDef(var_args)) {
          args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
        }
        return G__18174__delegate.call(this, args)
      };
      G__18174.cljs$lang$maxFixedArity = 0;
      G__18174.cljs$lang$applyTo = function(arglist__18175) {
        var args = cljs.core.seq(arglist__18175);
        return G__18174__delegate(args)
      };
      G__18174.cljs$lang$arity$variadic = G__18174__delegate;
      return G__18174
    }()
  };
  var partial__3 = function(f, arg1, arg2) {
    return function() {
      var G__18176__delegate = function(args) {
        return cljs.core.apply.call(null, f, arg1, arg2, args)
      };
      var G__18176 = function(var_args) {
        var args = null;
        if(goog.isDef(var_args)) {
          args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
        }
        return G__18176__delegate.call(this, args)
      };
      G__18176.cljs$lang$maxFixedArity = 0;
      G__18176.cljs$lang$applyTo = function(arglist__18177) {
        var args = cljs.core.seq(arglist__18177);
        return G__18176__delegate(args)
      };
      G__18176.cljs$lang$arity$variadic = G__18176__delegate;
      return G__18176
    }()
  };
  var partial__4 = function(f, arg1, arg2, arg3) {
    return function() {
      var G__18178__delegate = function(args) {
        return cljs.core.apply.call(null, f, arg1, arg2, arg3, args)
      };
      var G__18178 = function(var_args) {
        var args = null;
        if(goog.isDef(var_args)) {
          args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
        }
        return G__18178__delegate.call(this, args)
      };
      G__18178.cljs$lang$maxFixedArity = 0;
      G__18178.cljs$lang$applyTo = function(arglist__18179) {
        var args = cljs.core.seq(arglist__18179);
        return G__18178__delegate(args)
      };
      G__18178.cljs$lang$arity$variadic = G__18178__delegate;
      return G__18178
    }()
  };
  var partial__5 = function() {
    var G__18180__delegate = function(f, arg1, arg2, arg3, more) {
      return function() {
        var G__18181__delegate = function(args) {
          return cljs.core.apply.call(null, f, arg1, arg2, arg3, cljs.core.concat.call(null, more, args))
        };
        var G__18181 = function(var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
          }
          return G__18181__delegate.call(this, args)
        };
        G__18181.cljs$lang$maxFixedArity = 0;
        G__18181.cljs$lang$applyTo = function(arglist__18182) {
          var args = cljs.core.seq(arglist__18182);
          return G__18181__delegate(args)
        };
        G__18181.cljs$lang$arity$variadic = G__18181__delegate;
        return G__18181
      }()
    };
    var G__18180 = function(f, arg1, arg2, arg3, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__18180__delegate.call(this, f, arg1, arg2, arg3, more)
    };
    G__18180.cljs$lang$maxFixedArity = 4;
    G__18180.cljs$lang$applyTo = function(arglist__18183) {
      var f = cljs.core.first(arglist__18183);
      var arg1 = cljs.core.first(cljs.core.next(arglist__18183));
      var arg2 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18183)));
      var arg3 = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18183))));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18183))));
      return G__18180__delegate(f, arg1, arg2, arg3, more)
    };
    G__18180.cljs$lang$arity$variadic = G__18180__delegate;
    return G__18180
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
      var G__18184 = null;
      var G__18184__1 = function(a) {
        return f.call(null, a == null ? x : a)
      };
      var G__18184__2 = function(a, b) {
        return f.call(null, a == null ? x : a, b)
      };
      var G__18184__3 = function(a, b, c) {
        return f.call(null, a == null ? x : a, b, c)
      };
      var G__18184__4 = function() {
        var G__18185__delegate = function(a, b, c, ds) {
          return cljs.core.apply.call(null, f, a == null ? x : a, b, c, ds)
        };
        var G__18185 = function(a, b, c, var_args) {
          var ds = null;
          if(goog.isDef(var_args)) {
            ds = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18185__delegate.call(this, a, b, c, ds)
        };
        G__18185.cljs$lang$maxFixedArity = 3;
        G__18185.cljs$lang$applyTo = function(arglist__18186) {
          var a = cljs.core.first(arglist__18186);
          var b = cljs.core.first(cljs.core.next(arglist__18186));
          var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18186)));
          var ds = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18186)));
          return G__18185__delegate(a, b, c, ds)
        };
        G__18185.cljs$lang$arity$variadic = G__18185__delegate;
        return G__18185
      }();
      G__18184 = function(a, b, c, var_args) {
        var ds = var_args;
        switch(arguments.length) {
          case 1:
            return G__18184__1.call(this, a);
          case 2:
            return G__18184__2.call(this, a, b);
          case 3:
            return G__18184__3.call(this, a, b, c);
          default:
            return G__18184__4.cljs$lang$arity$variadic(a, b, c, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__18184.cljs$lang$maxFixedArity = 3;
      G__18184.cljs$lang$applyTo = G__18184__4.cljs$lang$applyTo;
      return G__18184
    }()
  };
  var fnil__3 = function(f, x, y) {
    return function() {
      var G__18187 = null;
      var G__18187__2 = function(a, b) {
        return f.call(null, a == null ? x : a, b == null ? y : b)
      };
      var G__18187__3 = function(a, b, c) {
        return f.call(null, a == null ? x : a, b == null ? y : b, c)
      };
      var G__18187__4 = function() {
        var G__18188__delegate = function(a, b, c, ds) {
          return cljs.core.apply.call(null, f, a == null ? x : a, b == null ? y : b, c, ds)
        };
        var G__18188 = function(a, b, c, var_args) {
          var ds = null;
          if(goog.isDef(var_args)) {
            ds = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18188__delegate.call(this, a, b, c, ds)
        };
        G__18188.cljs$lang$maxFixedArity = 3;
        G__18188.cljs$lang$applyTo = function(arglist__18189) {
          var a = cljs.core.first(arglist__18189);
          var b = cljs.core.first(cljs.core.next(arglist__18189));
          var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18189)));
          var ds = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18189)));
          return G__18188__delegate(a, b, c, ds)
        };
        G__18188.cljs$lang$arity$variadic = G__18188__delegate;
        return G__18188
      }();
      G__18187 = function(a, b, c, var_args) {
        var ds = var_args;
        switch(arguments.length) {
          case 2:
            return G__18187__2.call(this, a, b);
          case 3:
            return G__18187__3.call(this, a, b, c);
          default:
            return G__18187__4.cljs$lang$arity$variadic(a, b, c, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__18187.cljs$lang$maxFixedArity = 3;
      G__18187.cljs$lang$applyTo = G__18187__4.cljs$lang$applyTo;
      return G__18187
    }()
  };
  var fnil__4 = function(f, x, y, z) {
    return function() {
      var G__18190 = null;
      var G__18190__2 = function(a, b) {
        return f.call(null, a == null ? x : a, b == null ? y : b)
      };
      var G__18190__3 = function(a, b, c) {
        return f.call(null, a == null ? x : a, b == null ? y : b, c == null ? z : c)
      };
      var G__18190__4 = function() {
        var G__18191__delegate = function(a, b, c, ds) {
          return cljs.core.apply.call(null, f, a == null ? x : a, b == null ? y : b, c == null ? z : c, ds)
        };
        var G__18191 = function(a, b, c, var_args) {
          var ds = null;
          if(goog.isDef(var_args)) {
            ds = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18191__delegate.call(this, a, b, c, ds)
        };
        G__18191.cljs$lang$maxFixedArity = 3;
        G__18191.cljs$lang$applyTo = function(arglist__18192) {
          var a = cljs.core.first(arglist__18192);
          var b = cljs.core.first(cljs.core.next(arglist__18192));
          var c = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18192)));
          var ds = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18192)));
          return G__18191__delegate(a, b, c, ds)
        };
        G__18191.cljs$lang$arity$variadic = G__18191__delegate;
        return G__18191
      }();
      G__18190 = function(a, b, c, var_args) {
        var ds = var_args;
        switch(arguments.length) {
          case 2:
            return G__18190__2.call(this, a, b);
          case 3:
            return G__18190__3.call(this, a, b, c);
          default:
            return G__18190__4.cljs$lang$arity$variadic(a, b, c, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__18190.cljs$lang$maxFixedArity = 3;
      G__18190.cljs$lang$applyTo = G__18190__4.cljs$lang$applyTo;
      return G__18190
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
  var mapi__18208 = function mapi(idx, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____18216 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18216) {
        var s__18217 = temp__3974__auto____18216;
        if(cljs.core.chunked_seq_QMARK_.call(null, s__18217)) {
          var c__18218 = cljs.core.chunk_first.call(null, s__18217);
          var size__18219 = cljs.core.count.call(null, c__18218);
          var b__18220 = cljs.core.chunk_buffer.call(null, size__18219);
          var n__2593__auto____18221 = size__18219;
          var i__18222 = 0;
          while(true) {
            if(i__18222 < n__2593__auto____18221) {
              cljs.core.chunk_append.call(null, b__18220, f.call(null, idx + i__18222, cljs.core._nth.call(null, c__18218, i__18222)));
              var G__18223 = i__18222 + 1;
              i__18222 = G__18223;
              continue
            }else {
            }
            break
          }
          return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18220), mapi.call(null, idx + size__18219, cljs.core.chunk_rest.call(null, s__18217)))
        }else {
          return cljs.core.cons.call(null, f.call(null, idx, cljs.core.first.call(null, s__18217)), mapi.call(null, idx + 1, cljs.core.rest.call(null, s__18217)))
        }
      }else {
        return null
      }
    }, null)
  };
  return mapi__18208.call(null, 0, coll)
};
cljs.core.keep = function keep(f, coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____18233 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____18233) {
      var s__18234 = temp__3974__auto____18233;
      if(cljs.core.chunked_seq_QMARK_.call(null, s__18234)) {
        var c__18235 = cljs.core.chunk_first.call(null, s__18234);
        var size__18236 = cljs.core.count.call(null, c__18235);
        var b__18237 = cljs.core.chunk_buffer.call(null, size__18236);
        var n__2593__auto____18238 = size__18236;
        var i__18239 = 0;
        while(true) {
          if(i__18239 < n__2593__auto____18238) {
            var x__18240 = f.call(null, cljs.core._nth.call(null, c__18235, i__18239));
            if(x__18240 == null) {
            }else {
              cljs.core.chunk_append.call(null, b__18237, x__18240)
            }
            var G__18242 = i__18239 + 1;
            i__18239 = G__18242;
            continue
          }else {
          }
          break
        }
        return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18237), keep.call(null, f, cljs.core.chunk_rest.call(null, s__18234)))
      }else {
        var x__18241 = f.call(null, cljs.core.first.call(null, s__18234));
        if(x__18241 == null) {
          return keep.call(null, f, cljs.core.rest.call(null, s__18234))
        }else {
          return cljs.core.cons.call(null, x__18241, keep.call(null, f, cljs.core.rest.call(null, s__18234)))
        }
      }
    }else {
      return null
    }
  }, null)
};
cljs.core.keep_indexed = function keep_indexed(f, coll) {
  var keepi__18268 = function keepi(idx, coll) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____18278 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18278) {
        var s__18279 = temp__3974__auto____18278;
        if(cljs.core.chunked_seq_QMARK_.call(null, s__18279)) {
          var c__18280 = cljs.core.chunk_first.call(null, s__18279);
          var size__18281 = cljs.core.count.call(null, c__18280);
          var b__18282 = cljs.core.chunk_buffer.call(null, size__18281);
          var n__2593__auto____18283 = size__18281;
          var i__18284 = 0;
          while(true) {
            if(i__18284 < n__2593__auto____18283) {
              var x__18285 = f.call(null, idx + i__18284, cljs.core._nth.call(null, c__18280, i__18284));
              if(x__18285 == null) {
              }else {
                cljs.core.chunk_append.call(null, b__18282, x__18285)
              }
              var G__18287 = i__18284 + 1;
              i__18284 = G__18287;
              continue
            }else {
            }
            break
          }
          return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18282), keepi.call(null, idx + size__18281, cljs.core.chunk_rest.call(null, s__18279)))
        }else {
          var x__18286 = f.call(null, idx, cljs.core.first.call(null, s__18279));
          if(x__18286 == null) {
            return keepi.call(null, idx + 1, cljs.core.rest.call(null, s__18279))
          }else {
            return cljs.core.cons.call(null, x__18286, keepi.call(null, idx + 1, cljs.core.rest.call(null, s__18279)))
          }
        }
      }else {
        return null
      }
    }, null)
  };
  return keepi__18268.call(null, 0, coll)
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
          var and__3822__auto____18373 = p.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18373)) {
            return p.call(null, y)
          }else {
            return and__3822__auto____18373
          }
        }())
      };
      var ep1__3 = function(x, y, z) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18374 = p.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18374)) {
            var and__3822__auto____18375 = p.call(null, y);
            if(cljs.core.truth_(and__3822__auto____18375)) {
              return p.call(null, z)
            }else {
              return and__3822__auto____18375
            }
          }else {
            return and__3822__auto____18374
          }
        }())
      };
      var ep1__4 = function() {
        var G__18444__delegate = function(x, y, z, args) {
          return cljs.core.boolean$.call(null, function() {
            var and__3822__auto____18376 = ep1.call(null, x, y, z);
            if(cljs.core.truth_(and__3822__auto____18376)) {
              return cljs.core.every_QMARK_.call(null, p, args)
            }else {
              return and__3822__auto____18376
            }
          }())
        };
        var G__18444 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18444__delegate.call(this, x, y, z, args)
        };
        G__18444.cljs$lang$maxFixedArity = 3;
        G__18444.cljs$lang$applyTo = function(arglist__18445) {
          var x = cljs.core.first(arglist__18445);
          var y = cljs.core.first(cljs.core.next(arglist__18445));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18445)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18445)));
          return G__18444__delegate(x, y, z, args)
        };
        G__18444.cljs$lang$arity$variadic = G__18444__delegate;
        return G__18444
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
          var and__3822__auto____18388 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18388)) {
            return p2.call(null, x)
          }else {
            return and__3822__auto____18388
          }
        }())
      };
      var ep2__2 = function(x, y) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18389 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18389)) {
            var and__3822__auto____18390 = p1.call(null, y);
            if(cljs.core.truth_(and__3822__auto____18390)) {
              var and__3822__auto____18391 = p2.call(null, x);
              if(cljs.core.truth_(and__3822__auto____18391)) {
                return p2.call(null, y)
              }else {
                return and__3822__auto____18391
              }
            }else {
              return and__3822__auto____18390
            }
          }else {
            return and__3822__auto____18389
          }
        }())
      };
      var ep2__3 = function(x, y, z) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18392 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18392)) {
            var and__3822__auto____18393 = p1.call(null, y);
            if(cljs.core.truth_(and__3822__auto____18393)) {
              var and__3822__auto____18394 = p1.call(null, z);
              if(cljs.core.truth_(and__3822__auto____18394)) {
                var and__3822__auto____18395 = p2.call(null, x);
                if(cljs.core.truth_(and__3822__auto____18395)) {
                  var and__3822__auto____18396 = p2.call(null, y);
                  if(cljs.core.truth_(and__3822__auto____18396)) {
                    return p2.call(null, z)
                  }else {
                    return and__3822__auto____18396
                  }
                }else {
                  return and__3822__auto____18395
                }
              }else {
                return and__3822__auto____18394
              }
            }else {
              return and__3822__auto____18393
            }
          }else {
            return and__3822__auto____18392
          }
        }())
      };
      var ep2__4 = function() {
        var G__18446__delegate = function(x, y, z, args) {
          return cljs.core.boolean$.call(null, function() {
            var and__3822__auto____18397 = ep2.call(null, x, y, z);
            if(cljs.core.truth_(and__3822__auto____18397)) {
              return cljs.core.every_QMARK_.call(null, function(p1__18243_SHARP_) {
                var and__3822__auto____18398 = p1.call(null, p1__18243_SHARP_);
                if(cljs.core.truth_(and__3822__auto____18398)) {
                  return p2.call(null, p1__18243_SHARP_)
                }else {
                  return and__3822__auto____18398
                }
              }, args)
            }else {
              return and__3822__auto____18397
            }
          }())
        };
        var G__18446 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18446__delegate.call(this, x, y, z, args)
        };
        G__18446.cljs$lang$maxFixedArity = 3;
        G__18446.cljs$lang$applyTo = function(arglist__18447) {
          var x = cljs.core.first(arglist__18447);
          var y = cljs.core.first(cljs.core.next(arglist__18447));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18447)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18447)));
          return G__18446__delegate(x, y, z, args)
        };
        G__18446.cljs$lang$arity$variadic = G__18446__delegate;
        return G__18446
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
          var and__3822__auto____18417 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18417)) {
            var and__3822__auto____18418 = p2.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18418)) {
              return p3.call(null, x)
            }else {
              return and__3822__auto____18418
            }
          }else {
            return and__3822__auto____18417
          }
        }())
      };
      var ep3__2 = function(x, y) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18419 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18419)) {
            var and__3822__auto____18420 = p2.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18420)) {
              var and__3822__auto____18421 = p3.call(null, x);
              if(cljs.core.truth_(and__3822__auto____18421)) {
                var and__3822__auto____18422 = p1.call(null, y);
                if(cljs.core.truth_(and__3822__auto____18422)) {
                  var and__3822__auto____18423 = p2.call(null, y);
                  if(cljs.core.truth_(and__3822__auto____18423)) {
                    return p3.call(null, y)
                  }else {
                    return and__3822__auto____18423
                  }
                }else {
                  return and__3822__auto____18422
                }
              }else {
                return and__3822__auto____18421
              }
            }else {
              return and__3822__auto____18420
            }
          }else {
            return and__3822__auto____18419
          }
        }())
      };
      var ep3__3 = function(x, y, z) {
        return cljs.core.boolean$.call(null, function() {
          var and__3822__auto____18424 = p1.call(null, x);
          if(cljs.core.truth_(and__3822__auto____18424)) {
            var and__3822__auto____18425 = p2.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18425)) {
              var and__3822__auto____18426 = p3.call(null, x);
              if(cljs.core.truth_(and__3822__auto____18426)) {
                var and__3822__auto____18427 = p1.call(null, y);
                if(cljs.core.truth_(and__3822__auto____18427)) {
                  var and__3822__auto____18428 = p2.call(null, y);
                  if(cljs.core.truth_(and__3822__auto____18428)) {
                    var and__3822__auto____18429 = p3.call(null, y);
                    if(cljs.core.truth_(and__3822__auto____18429)) {
                      var and__3822__auto____18430 = p1.call(null, z);
                      if(cljs.core.truth_(and__3822__auto____18430)) {
                        var and__3822__auto____18431 = p2.call(null, z);
                        if(cljs.core.truth_(and__3822__auto____18431)) {
                          return p3.call(null, z)
                        }else {
                          return and__3822__auto____18431
                        }
                      }else {
                        return and__3822__auto____18430
                      }
                    }else {
                      return and__3822__auto____18429
                    }
                  }else {
                    return and__3822__auto____18428
                  }
                }else {
                  return and__3822__auto____18427
                }
              }else {
                return and__3822__auto____18426
              }
            }else {
              return and__3822__auto____18425
            }
          }else {
            return and__3822__auto____18424
          }
        }())
      };
      var ep3__4 = function() {
        var G__18448__delegate = function(x, y, z, args) {
          return cljs.core.boolean$.call(null, function() {
            var and__3822__auto____18432 = ep3.call(null, x, y, z);
            if(cljs.core.truth_(and__3822__auto____18432)) {
              return cljs.core.every_QMARK_.call(null, function(p1__18244_SHARP_) {
                var and__3822__auto____18433 = p1.call(null, p1__18244_SHARP_);
                if(cljs.core.truth_(and__3822__auto____18433)) {
                  var and__3822__auto____18434 = p2.call(null, p1__18244_SHARP_);
                  if(cljs.core.truth_(and__3822__auto____18434)) {
                    return p3.call(null, p1__18244_SHARP_)
                  }else {
                    return and__3822__auto____18434
                  }
                }else {
                  return and__3822__auto____18433
                }
              }, args)
            }else {
              return and__3822__auto____18432
            }
          }())
        };
        var G__18448 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18448__delegate.call(this, x, y, z, args)
        };
        G__18448.cljs$lang$maxFixedArity = 3;
        G__18448.cljs$lang$applyTo = function(arglist__18449) {
          var x = cljs.core.first(arglist__18449);
          var y = cljs.core.first(cljs.core.next(arglist__18449));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18449)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18449)));
          return G__18448__delegate(x, y, z, args)
        };
        G__18448.cljs$lang$arity$variadic = G__18448__delegate;
        return G__18448
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
    var G__18450__delegate = function(p1, p2, p3, ps) {
      var ps__18435 = cljs.core.list_STAR_.call(null, p1, p2, p3, ps);
      return function() {
        var epn = null;
        var epn__0 = function() {
          return true
        };
        var epn__1 = function(x) {
          return cljs.core.every_QMARK_.call(null, function(p1__18245_SHARP_) {
            return p1__18245_SHARP_.call(null, x)
          }, ps__18435)
        };
        var epn__2 = function(x, y) {
          return cljs.core.every_QMARK_.call(null, function(p1__18246_SHARP_) {
            var and__3822__auto____18440 = p1__18246_SHARP_.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18440)) {
              return p1__18246_SHARP_.call(null, y)
            }else {
              return and__3822__auto____18440
            }
          }, ps__18435)
        };
        var epn__3 = function(x, y, z) {
          return cljs.core.every_QMARK_.call(null, function(p1__18247_SHARP_) {
            var and__3822__auto____18441 = p1__18247_SHARP_.call(null, x);
            if(cljs.core.truth_(and__3822__auto____18441)) {
              var and__3822__auto____18442 = p1__18247_SHARP_.call(null, y);
              if(cljs.core.truth_(and__3822__auto____18442)) {
                return p1__18247_SHARP_.call(null, z)
              }else {
                return and__3822__auto____18442
              }
            }else {
              return and__3822__auto____18441
            }
          }, ps__18435)
        };
        var epn__4 = function() {
          var G__18451__delegate = function(x, y, z, args) {
            return cljs.core.boolean$.call(null, function() {
              var and__3822__auto____18443 = epn.call(null, x, y, z);
              if(cljs.core.truth_(and__3822__auto____18443)) {
                return cljs.core.every_QMARK_.call(null, function(p1__18248_SHARP_) {
                  return cljs.core.every_QMARK_.call(null, p1__18248_SHARP_, args)
                }, ps__18435)
              }else {
                return and__3822__auto____18443
              }
            }())
          };
          var G__18451 = function(x, y, z, var_args) {
            var args = null;
            if(goog.isDef(var_args)) {
              args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
            }
            return G__18451__delegate.call(this, x, y, z, args)
          };
          G__18451.cljs$lang$maxFixedArity = 3;
          G__18451.cljs$lang$applyTo = function(arglist__18452) {
            var x = cljs.core.first(arglist__18452);
            var y = cljs.core.first(cljs.core.next(arglist__18452));
            var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18452)));
            var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18452)));
            return G__18451__delegate(x, y, z, args)
          };
          G__18451.cljs$lang$arity$variadic = G__18451__delegate;
          return G__18451
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
    var G__18450 = function(p1, p2, p3, var_args) {
      var ps = null;
      if(goog.isDef(var_args)) {
        ps = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__18450__delegate.call(this, p1, p2, p3, ps)
    };
    G__18450.cljs$lang$maxFixedArity = 3;
    G__18450.cljs$lang$applyTo = function(arglist__18453) {
      var p1 = cljs.core.first(arglist__18453);
      var p2 = cljs.core.first(cljs.core.next(arglist__18453));
      var p3 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18453)));
      var ps = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18453)));
      return G__18450__delegate(p1, p2, p3, ps)
    };
    G__18450.cljs$lang$arity$variadic = G__18450__delegate;
    return G__18450
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
        var or__3824__auto____18534 = p.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18534)) {
          return or__3824__auto____18534
        }else {
          return p.call(null, y)
        }
      };
      var sp1__3 = function(x, y, z) {
        var or__3824__auto____18535 = p.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18535)) {
          return or__3824__auto____18535
        }else {
          var or__3824__auto____18536 = p.call(null, y);
          if(cljs.core.truth_(or__3824__auto____18536)) {
            return or__3824__auto____18536
          }else {
            return p.call(null, z)
          }
        }
      };
      var sp1__4 = function() {
        var G__18605__delegate = function(x, y, z, args) {
          var or__3824__auto____18537 = sp1.call(null, x, y, z);
          if(cljs.core.truth_(or__3824__auto____18537)) {
            return or__3824__auto____18537
          }else {
            return cljs.core.some.call(null, p, args)
          }
        };
        var G__18605 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18605__delegate.call(this, x, y, z, args)
        };
        G__18605.cljs$lang$maxFixedArity = 3;
        G__18605.cljs$lang$applyTo = function(arglist__18606) {
          var x = cljs.core.first(arglist__18606);
          var y = cljs.core.first(cljs.core.next(arglist__18606));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18606)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18606)));
          return G__18605__delegate(x, y, z, args)
        };
        G__18605.cljs$lang$arity$variadic = G__18605__delegate;
        return G__18605
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
        var or__3824__auto____18549 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18549)) {
          return or__3824__auto____18549
        }else {
          return p2.call(null, x)
        }
      };
      var sp2__2 = function(x, y) {
        var or__3824__auto____18550 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18550)) {
          return or__3824__auto____18550
        }else {
          var or__3824__auto____18551 = p1.call(null, y);
          if(cljs.core.truth_(or__3824__auto____18551)) {
            return or__3824__auto____18551
          }else {
            var or__3824__auto____18552 = p2.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18552)) {
              return or__3824__auto____18552
            }else {
              return p2.call(null, y)
            }
          }
        }
      };
      var sp2__3 = function(x, y, z) {
        var or__3824__auto____18553 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18553)) {
          return or__3824__auto____18553
        }else {
          var or__3824__auto____18554 = p1.call(null, y);
          if(cljs.core.truth_(or__3824__auto____18554)) {
            return or__3824__auto____18554
          }else {
            var or__3824__auto____18555 = p1.call(null, z);
            if(cljs.core.truth_(or__3824__auto____18555)) {
              return or__3824__auto____18555
            }else {
              var or__3824__auto____18556 = p2.call(null, x);
              if(cljs.core.truth_(or__3824__auto____18556)) {
                return or__3824__auto____18556
              }else {
                var or__3824__auto____18557 = p2.call(null, y);
                if(cljs.core.truth_(or__3824__auto____18557)) {
                  return or__3824__auto____18557
                }else {
                  return p2.call(null, z)
                }
              }
            }
          }
        }
      };
      var sp2__4 = function() {
        var G__18607__delegate = function(x, y, z, args) {
          var or__3824__auto____18558 = sp2.call(null, x, y, z);
          if(cljs.core.truth_(or__3824__auto____18558)) {
            return or__3824__auto____18558
          }else {
            return cljs.core.some.call(null, function(p1__18288_SHARP_) {
              var or__3824__auto____18559 = p1.call(null, p1__18288_SHARP_);
              if(cljs.core.truth_(or__3824__auto____18559)) {
                return or__3824__auto____18559
              }else {
                return p2.call(null, p1__18288_SHARP_)
              }
            }, args)
          }
        };
        var G__18607 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18607__delegate.call(this, x, y, z, args)
        };
        G__18607.cljs$lang$maxFixedArity = 3;
        G__18607.cljs$lang$applyTo = function(arglist__18608) {
          var x = cljs.core.first(arglist__18608);
          var y = cljs.core.first(cljs.core.next(arglist__18608));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18608)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18608)));
          return G__18607__delegate(x, y, z, args)
        };
        G__18607.cljs$lang$arity$variadic = G__18607__delegate;
        return G__18607
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
        var or__3824__auto____18578 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18578)) {
          return or__3824__auto____18578
        }else {
          var or__3824__auto____18579 = p2.call(null, x);
          if(cljs.core.truth_(or__3824__auto____18579)) {
            return or__3824__auto____18579
          }else {
            return p3.call(null, x)
          }
        }
      };
      var sp3__2 = function(x, y) {
        var or__3824__auto____18580 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18580)) {
          return or__3824__auto____18580
        }else {
          var or__3824__auto____18581 = p2.call(null, x);
          if(cljs.core.truth_(or__3824__auto____18581)) {
            return or__3824__auto____18581
          }else {
            var or__3824__auto____18582 = p3.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18582)) {
              return or__3824__auto____18582
            }else {
              var or__3824__auto____18583 = p1.call(null, y);
              if(cljs.core.truth_(or__3824__auto____18583)) {
                return or__3824__auto____18583
              }else {
                var or__3824__auto____18584 = p2.call(null, y);
                if(cljs.core.truth_(or__3824__auto____18584)) {
                  return or__3824__auto____18584
                }else {
                  return p3.call(null, y)
                }
              }
            }
          }
        }
      };
      var sp3__3 = function(x, y, z) {
        var or__3824__auto____18585 = p1.call(null, x);
        if(cljs.core.truth_(or__3824__auto____18585)) {
          return or__3824__auto____18585
        }else {
          var or__3824__auto____18586 = p2.call(null, x);
          if(cljs.core.truth_(or__3824__auto____18586)) {
            return or__3824__auto____18586
          }else {
            var or__3824__auto____18587 = p3.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18587)) {
              return or__3824__auto____18587
            }else {
              var or__3824__auto____18588 = p1.call(null, y);
              if(cljs.core.truth_(or__3824__auto____18588)) {
                return or__3824__auto____18588
              }else {
                var or__3824__auto____18589 = p2.call(null, y);
                if(cljs.core.truth_(or__3824__auto____18589)) {
                  return or__3824__auto____18589
                }else {
                  var or__3824__auto____18590 = p3.call(null, y);
                  if(cljs.core.truth_(or__3824__auto____18590)) {
                    return or__3824__auto____18590
                  }else {
                    var or__3824__auto____18591 = p1.call(null, z);
                    if(cljs.core.truth_(or__3824__auto____18591)) {
                      return or__3824__auto____18591
                    }else {
                      var or__3824__auto____18592 = p2.call(null, z);
                      if(cljs.core.truth_(or__3824__auto____18592)) {
                        return or__3824__auto____18592
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
        var G__18609__delegate = function(x, y, z, args) {
          var or__3824__auto____18593 = sp3.call(null, x, y, z);
          if(cljs.core.truth_(or__3824__auto____18593)) {
            return or__3824__auto____18593
          }else {
            return cljs.core.some.call(null, function(p1__18289_SHARP_) {
              var or__3824__auto____18594 = p1.call(null, p1__18289_SHARP_);
              if(cljs.core.truth_(or__3824__auto____18594)) {
                return or__3824__auto____18594
              }else {
                var or__3824__auto____18595 = p2.call(null, p1__18289_SHARP_);
                if(cljs.core.truth_(or__3824__auto____18595)) {
                  return or__3824__auto____18595
                }else {
                  return p3.call(null, p1__18289_SHARP_)
                }
              }
            }, args)
          }
        };
        var G__18609 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__18609__delegate.call(this, x, y, z, args)
        };
        G__18609.cljs$lang$maxFixedArity = 3;
        G__18609.cljs$lang$applyTo = function(arglist__18610) {
          var x = cljs.core.first(arglist__18610);
          var y = cljs.core.first(cljs.core.next(arglist__18610));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18610)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18610)));
          return G__18609__delegate(x, y, z, args)
        };
        G__18609.cljs$lang$arity$variadic = G__18609__delegate;
        return G__18609
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
    var G__18611__delegate = function(p1, p2, p3, ps) {
      var ps__18596 = cljs.core.list_STAR_.call(null, p1, p2, p3, ps);
      return function() {
        var spn = null;
        var spn__0 = function() {
          return null
        };
        var spn__1 = function(x) {
          return cljs.core.some.call(null, function(p1__18290_SHARP_) {
            return p1__18290_SHARP_.call(null, x)
          }, ps__18596)
        };
        var spn__2 = function(x, y) {
          return cljs.core.some.call(null, function(p1__18291_SHARP_) {
            var or__3824__auto____18601 = p1__18291_SHARP_.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18601)) {
              return or__3824__auto____18601
            }else {
              return p1__18291_SHARP_.call(null, y)
            }
          }, ps__18596)
        };
        var spn__3 = function(x, y, z) {
          return cljs.core.some.call(null, function(p1__18292_SHARP_) {
            var or__3824__auto____18602 = p1__18292_SHARP_.call(null, x);
            if(cljs.core.truth_(or__3824__auto____18602)) {
              return or__3824__auto____18602
            }else {
              var or__3824__auto____18603 = p1__18292_SHARP_.call(null, y);
              if(cljs.core.truth_(or__3824__auto____18603)) {
                return or__3824__auto____18603
              }else {
                return p1__18292_SHARP_.call(null, z)
              }
            }
          }, ps__18596)
        };
        var spn__4 = function() {
          var G__18612__delegate = function(x, y, z, args) {
            var or__3824__auto____18604 = spn.call(null, x, y, z);
            if(cljs.core.truth_(or__3824__auto____18604)) {
              return or__3824__auto____18604
            }else {
              return cljs.core.some.call(null, function(p1__18293_SHARP_) {
                return cljs.core.some.call(null, p1__18293_SHARP_, args)
              }, ps__18596)
            }
          };
          var G__18612 = function(x, y, z, var_args) {
            var args = null;
            if(goog.isDef(var_args)) {
              args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
            }
            return G__18612__delegate.call(this, x, y, z, args)
          };
          G__18612.cljs$lang$maxFixedArity = 3;
          G__18612.cljs$lang$applyTo = function(arglist__18613) {
            var x = cljs.core.first(arglist__18613);
            var y = cljs.core.first(cljs.core.next(arglist__18613));
            var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18613)));
            var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18613)));
            return G__18612__delegate(x, y, z, args)
          };
          G__18612.cljs$lang$arity$variadic = G__18612__delegate;
          return G__18612
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
    var G__18611 = function(p1, p2, p3, var_args) {
      var ps = null;
      if(goog.isDef(var_args)) {
        ps = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__18611__delegate.call(this, p1, p2, p3, ps)
    };
    G__18611.cljs$lang$maxFixedArity = 3;
    G__18611.cljs$lang$applyTo = function(arglist__18614) {
      var p1 = cljs.core.first(arglist__18614);
      var p2 = cljs.core.first(cljs.core.next(arglist__18614));
      var p3 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18614)));
      var ps = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18614)));
      return G__18611__delegate(p1, p2, p3, ps)
    };
    G__18611.cljs$lang$arity$variadic = G__18611__delegate;
    return G__18611
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
      var temp__3974__auto____18633 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18633) {
        var s__18634 = temp__3974__auto____18633;
        if(cljs.core.chunked_seq_QMARK_.call(null, s__18634)) {
          var c__18635 = cljs.core.chunk_first.call(null, s__18634);
          var size__18636 = cljs.core.count.call(null, c__18635);
          var b__18637 = cljs.core.chunk_buffer.call(null, size__18636);
          var n__2593__auto____18638 = size__18636;
          var i__18639 = 0;
          while(true) {
            if(i__18639 < n__2593__auto____18638) {
              cljs.core.chunk_append.call(null, b__18637, f.call(null, cljs.core._nth.call(null, c__18635, i__18639)));
              var G__18651 = i__18639 + 1;
              i__18639 = G__18651;
              continue
            }else {
            }
            break
          }
          return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18637), map.call(null, f, cljs.core.chunk_rest.call(null, s__18634)))
        }else {
          return cljs.core.cons.call(null, f.call(null, cljs.core.first.call(null, s__18634)), map.call(null, f, cljs.core.rest.call(null, s__18634)))
        }
      }else {
        return null
      }
    }, null)
  };
  var map__3 = function(f, c1, c2) {
    return new cljs.core.LazySeq(null, false, function() {
      var s1__18640 = cljs.core.seq.call(null, c1);
      var s2__18641 = cljs.core.seq.call(null, c2);
      if(function() {
        var and__3822__auto____18642 = s1__18640;
        if(and__3822__auto____18642) {
          return s2__18641
        }else {
          return and__3822__auto____18642
        }
      }()) {
        return cljs.core.cons.call(null, f.call(null, cljs.core.first.call(null, s1__18640), cljs.core.first.call(null, s2__18641)), map.call(null, f, cljs.core.rest.call(null, s1__18640), cljs.core.rest.call(null, s2__18641)))
      }else {
        return null
      }
    }, null)
  };
  var map__4 = function(f, c1, c2, c3) {
    return new cljs.core.LazySeq(null, false, function() {
      var s1__18643 = cljs.core.seq.call(null, c1);
      var s2__18644 = cljs.core.seq.call(null, c2);
      var s3__18645 = cljs.core.seq.call(null, c3);
      if(function() {
        var and__3822__auto____18646 = s1__18643;
        if(and__3822__auto____18646) {
          var and__3822__auto____18647 = s2__18644;
          if(and__3822__auto____18647) {
            return s3__18645
          }else {
            return and__3822__auto____18647
          }
        }else {
          return and__3822__auto____18646
        }
      }()) {
        return cljs.core.cons.call(null, f.call(null, cljs.core.first.call(null, s1__18643), cljs.core.first.call(null, s2__18644), cljs.core.first.call(null, s3__18645)), map.call(null, f, cljs.core.rest.call(null, s1__18643), cljs.core.rest.call(null, s2__18644), cljs.core.rest.call(null, s3__18645)))
      }else {
        return null
      }
    }, null)
  };
  var map__5 = function() {
    var G__18652__delegate = function(f, c1, c2, c3, colls) {
      var step__18650 = function step(cs) {
        return new cljs.core.LazySeq(null, false, function() {
          var ss__18649 = map.call(null, cljs.core.seq, cs);
          if(cljs.core.every_QMARK_.call(null, cljs.core.identity, ss__18649)) {
            return cljs.core.cons.call(null, map.call(null, cljs.core.first, ss__18649), step.call(null, map.call(null, cljs.core.rest, ss__18649)))
          }else {
            return null
          }
        }, null)
      };
      return map.call(null, function(p1__18454_SHARP_) {
        return cljs.core.apply.call(null, f, p1__18454_SHARP_)
      }, step__18650.call(null, cljs.core.conj.call(null, colls, c3, c2, c1)))
    };
    var G__18652 = function(f, c1, c2, c3, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__18652__delegate.call(this, f, c1, c2, c3, colls)
    };
    G__18652.cljs$lang$maxFixedArity = 4;
    G__18652.cljs$lang$applyTo = function(arglist__18653) {
      var f = cljs.core.first(arglist__18653);
      var c1 = cljs.core.first(cljs.core.next(arglist__18653));
      var c2 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18653)));
      var c3 = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18653))));
      var colls = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18653))));
      return G__18652__delegate(f, c1, c2, c3, colls)
    };
    G__18652.cljs$lang$arity$variadic = G__18652__delegate;
    return G__18652
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
      var temp__3974__auto____18656 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18656) {
        var s__18657 = temp__3974__auto____18656;
        return cljs.core.cons.call(null, cljs.core.first.call(null, s__18657), take.call(null, n - 1, cljs.core.rest.call(null, s__18657)))
      }else {
        return null
      }
    }else {
      return null
    }
  }, null)
};
cljs.core.drop = function drop(n, coll) {
  var step__18663 = function(n, coll) {
    while(true) {
      var s__18661 = cljs.core.seq.call(null, coll);
      if(cljs.core.truth_(function() {
        var and__3822__auto____18662 = n > 0;
        if(and__3822__auto____18662) {
          return s__18661
        }else {
          return and__3822__auto____18662
        }
      }())) {
        var G__18664 = n - 1;
        var G__18665 = cljs.core.rest.call(null, s__18661);
        n = G__18664;
        coll = G__18665;
        continue
      }else {
        return s__18661
      }
      break
    }
  };
  return new cljs.core.LazySeq(null, false, function() {
    return step__18663.call(null, n, coll)
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
  var s__18668 = cljs.core.seq.call(null, coll);
  var lead__18669 = cljs.core.seq.call(null, cljs.core.drop.call(null, n, coll));
  while(true) {
    if(lead__18669) {
      var G__18670 = cljs.core.next.call(null, s__18668);
      var G__18671 = cljs.core.next.call(null, lead__18669);
      s__18668 = G__18670;
      lead__18669 = G__18671;
      continue
    }else {
      return s__18668
    }
    break
  }
};
cljs.core.drop_while = function drop_while(pred, coll) {
  var step__18677 = function(pred, coll) {
    while(true) {
      var s__18675 = cljs.core.seq.call(null, coll);
      if(cljs.core.truth_(function() {
        var and__3822__auto____18676 = s__18675;
        if(and__3822__auto____18676) {
          return pred.call(null, cljs.core.first.call(null, s__18675))
        }else {
          return and__3822__auto____18676
        }
      }())) {
        var G__18678 = pred;
        var G__18679 = cljs.core.rest.call(null, s__18675);
        pred = G__18678;
        coll = G__18679;
        continue
      }else {
        return s__18675
      }
      break
    }
  };
  return new cljs.core.LazySeq(null, false, function() {
    return step__18677.call(null, pred, coll)
  }, null)
};
cljs.core.cycle = function cycle(coll) {
  return new cljs.core.LazySeq(null, false, function() {
    var temp__3974__auto____18682 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____18682) {
      var s__18683 = temp__3974__auto____18682;
      return cljs.core.concat.call(null, s__18683, cycle.call(null, s__18683))
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
      var s1__18688 = cljs.core.seq.call(null, c1);
      var s2__18689 = cljs.core.seq.call(null, c2);
      if(function() {
        var and__3822__auto____18690 = s1__18688;
        if(and__3822__auto____18690) {
          return s2__18689
        }else {
          return and__3822__auto____18690
        }
      }()) {
        return cljs.core.cons.call(null, cljs.core.first.call(null, s1__18688), cljs.core.cons.call(null, cljs.core.first.call(null, s2__18689), interleave.call(null, cljs.core.rest.call(null, s1__18688), cljs.core.rest.call(null, s2__18689))))
      }else {
        return null
      }
    }, null)
  };
  var interleave__3 = function() {
    var G__18692__delegate = function(c1, c2, colls) {
      return new cljs.core.LazySeq(null, false, function() {
        var ss__18691 = cljs.core.map.call(null, cljs.core.seq, cljs.core.conj.call(null, colls, c2, c1));
        if(cljs.core.every_QMARK_.call(null, cljs.core.identity, ss__18691)) {
          return cljs.core.concat.call(null, cljs.core.map.call(null, cljs.core.first, ss__18691), cljs.core.apply.call(null, interleave, cljs.core.map.call(null, cljs.core.rest, ss__18691)))
        }else {
          return null
        }
      }, null)
    };
    var G__18692 = function(c1, c2, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__18692__delegate.call(this, c1, c2, colls)
    };
    G__18692.cljs$lang$maxFixedArity = 2;
    G__18692.cljs$lang$applyTo = function(arglist__18693) {
      var c1 = cljs.core.first(arglist__18693);
      var c2 = cljs.core.first(cljs.core.next(arglist__18693));
      var colls = cljs.core.rest(cljs.core.next(arglist__18693));
      return G__18692__delegate(c1, c2, colls)
    };
    G__18692.cljs$lang$arity$variadic = G__18692__delegate;
    return G__18692
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
  var cat__18703 = function cat(coll, colls) {
    return new cljs.core.LazySeq(null, false, function() {
      var temp__3971__auto____18701 = cljs.core.seq.call(null, coll);
      if(temp__3971__auto____18701) {
        var coll__18702 = temp__3971__auto____18701;
        return cljs.core.cons.call(null, cljs.core.first.call(null, coll__18702), cat.call(null, cljs.core.rest.call(null, coll__18702), colls))
      }else {
        if(cljs.core.seq.call(null, colls)) {
          return cat.call(null, cljs.core.first.call(null, colls), cljs.core.rest.call(null, colls))
        }else {
          return null
        }
      }
    }, null)
  };
  return cat__18703.call(null, null, colls)
};
cljs.core.mapcat = function() {
  var mapcat = null;
  var mapcat__2 = function(f, coll) {
    return cljs.core.flatten1.call(null, cljs.core.map.call(null, f, coll))
  };
  var mapcat__3 = function() {
    var G__18704__delegate = function(f, coll, colls) {
      return cljs.core.flatten1.call(null, cljs.core.apply.call(null, cljs.core.map, f, coll, colls))
    };
    var G__18704 = function(f, coll, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__18704__delegate.call(this, f, coll, colls)
    };
    G__18704.cljs$lang$maxFixedArity = 2;
    G__18704.cljs$lang$applyTo = function(arglist__18705) {
      var f = cljs.core.first(arglist__18705);
      var coll = cljs.core.first(cljs.core.next(arglist__18705));
      var colls = cljs.core.rest(cljs.core.next(arglist__18705));
      return G__18704__delegate(f, coll, colls)
    };
    G__18704.cljs$lang$arity$variadic = G__18704__delegate;
    return G__18704
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
    var temp__3974__auto____18715 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____18715) {
      var s__18716 = temp__3974__auto____18715;
      if(cljs.core.chunked_seq_QMARK_.call(null, s__18716)) {
        var c__18717 = cljs.core.chunk_first.call(null, s__18716);
        var size__18718 = cljs.core.count.call(null, c__18717);
        var b__18719 = cljs.core.chunk_buffer.call(null, size__18718);
        var n__2593__auto____18720 = size__18718;
        var i__18721 = 0;
        while(true) {
          if(i__18721 < n__2593__auto____18720) {
            if(cljs.core.truth_(pred.call(null, cljs.core._nth.call(null, c__18717, i__18721)))) {
              cljs.core.chunk_append.call(null, b__18719, cljs.core._nth.call(null, c__18717, i__18721))
            }else {
            }
            var G__18724 = i__18721 + 1;
            i__18721 = G__18724;
            continue
          }else {
          }
          break
        }
        return cljs.core.chunk_cons.call(null, cljs.core.chunk.call(null, b__18719), filter.call(null, pred, cljs.core.chunk_rest.call(null, s__18716)))
      }else {
        var f__18722 = cljs.core.first.call(null, s__18716);
        var r__18723 = cljs.core.rest.call(null, s__18716);
        if(cljs.core.truth_(pred.call(null, f__18722))) {
          return cljs.core.cons.call(null, f__18722, filter.call(null, pred, r__18723))
        }else {
          return filter.call(null, pred, r__18723)
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
  var walk__18727 = function walk(node) {
    return new cljs.core.LazySeq(null, false, function() {
      return cljs.core.cons.call(null, node, cljs.core.truth_(branch_QMARK_.call(null, node)) ? cljs.core.mapcat.call(null, walk, children.call(null, node)) : null)
    }, null)
  };
  return walk__18727.call(null, root)
};
cljs.core.flatten = function flatten(x) {
  return cljs.core.filter.call(null, function(p1__18725_SHARP_) {
    return!cljs.core.sequential_QMARK_.call(null, p1__18725_SHARP_)
  }, cljs.core.rest.call(null, cljs.core.tree_seq.call(null, cljs.core.sequential_QMARK_, cljs.core.seq, x)))
};
cljs.core.into = function into(to, from) {
  if(function() {
    var G__18731__18732 = to;
    if(G__18731__18732) {
      if(function() {
        var or__3824__auto____18733 = G__18731__18732.cljs$lang$protocol_mask$partition1$ & 4;
        if(or__3824__auto____18733) {
          return or__3824__auto____18733
        }else {
          return G__18731__18732.cljs$core$IEditableCollection$
        }
      }()) {
        return true
      }else {
        if(!G__18731__18732.cljs$lang$protocol_mask$partition1$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IEditableCollection, G__18731__18732)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IEditableCollection, G__18731__18732)
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
    var G__18734__delegate = function(f, c1, c2, c3, colls) {
      return cljs.core.into.call(null, cljs.core.PersistentVector.EMPTY, cljs.core.apply.call(null, cljs.core.map, f, c1, c2, c3, colls))
    };
    var G__18734 = function(f, c1, c2, c3, var_args) {
      var colls = null;
      if(goog.isDef(var_args)) {
        colls = cljs.core.array_seq(Array.prototype.slice.call(arguments, 4), 0)
      }
      return G__18734__delegate.call(this, f, c1, c2, c3, colls)
    };
    G__18734.cljs$lang$maxFixedArity = 4;
    G__18734.cljs$lang$applyTo = function(arglist__18735) {
      var f = cljs.core.first(arglist__18735);
      var c1 = cljs.core.first(cljs.core.next(arglist__18735));
      var c2 = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18735)));
      var c3 = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18735))));
      var colls = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(arglist__18735))));
      return G__18734__delegate(f, c1, c2, c3, colls)
    };
    G__18734.cljs$lang$arity$variadic = G__18734__delegate;
    return G__18734
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
      var temp__3974__auto____18742 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18742) {
        var s__18743 = temp__3974__auto____18742;
        var p__18744 = cljs.core.take.call(null, n, s__18743);
        if(n === cljs.core.count.call(null, p__18744)) {
          return cljs.core.cons.call(null, p__18744, partition.call(null, n, step, cljs.core.drop.call(null, step, s__18743)))
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
      var temp__3974__auto____18745 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____18745) {
        var s__18746 = temp__3974__auto____18745;
        var p__18747 = cljs.core.take.call(null, n, s__18746);
        if(n === cljs.core.count.call(null, p__18747)) {
          return cljs.core.cons.call(null, p__18747, partition.call(null, n, step, pad, cljs.core.drop.call(null, step, s__18746)))
        }else {
          return cljs.core.list.call(null, cljs.core.take.call(null, n, cljs.core.concat.call(null, p__18747, pad)))
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
    var sentinel__18752 = cljs.core.lookup_sentinel;
    var m__18753 = m;
    var ks__18754 = cljs.core.seq.call(null, ks);
    while(true) {
      if(ks__18754) {
        var m__18755 = cljs.core._lookup.call(null, m__18753, cljs.core.first.call(null, ks__18754), sentinel__18752);
        if(sentinel__18752 === m__18755) {
          return not_found
        }else {
          var G__18756 = sentinel__18752;
          var G__18757 = m__18755;
          var G__18758 = cljs.core.next.call(null, ks__18754);
          sentinel__18752 = G__18756;
          m__18753 = G__18757;
          ks__18754 = G__18758;
          continue
        }
      }else {
        return m__18753
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
cljs.core.assoc_in = function assoc_in(m, p__18759, v) {
  var vec__18764__18765 = p__18759;
  var k__18766 = cljs.core.nth.call(null, vec__18764__18765, 0, null);
  var ks__18767 = cljs.core.nthnext.call(null, vec__18764__18765, 1);
  if(cljs.core.truth_(ks__18767)) {
    return cljs.core.assoc.call(null, m, k__18766, assoc_in.call(null, cljs.core._lookup.call(null, m, k__18766, null), ks__18767, v))
  }else {
    return cljs.core.assoc.call(null, m, k__18766, v)
  }
};
cljs.core.update_in = function() {
  var update_in__delegate = function(m, p__18768, f, args) {
    var vec__18773__18774 = p__18768;
    var k__18775 = cljs.core.nth.call(null, vec__18773__18774, 0, null);
    var ks__18776 = cljs.core.nthnext.call(null, vec__18773__18774, 1);
    if(cljs.core.truth_(ks__18776)) {
      return cljs.core.assoc.call(null, m, k__18775, cljs.core.apply.call(null, update_in, cljs.core._lookup.call(null, m, k__18775, null), ks__18776, f, args))
    }else {
      return cljs.core.assoc.call(null, m, k__18775, cljs.core.apply.call(null, f, cljs.core._lookup.call(null, m, k__18775, null), args))
    }
  };
  var update_in = function(m, p__18768, f, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
    }
    return update_in__delegate.call(this, m, p__18768, f, args)
  };
  update_in.cljs$lang$maxFixedArity = 3;
  update_in.cljs$lang$applyTo = function(arglist__18777) {
    var m = cljs.core.first(arglist__18777);
    var p__18768 = cljs.core.first(cljs.core.next(arglist__18777));
    var f = cljs.core.first(cljs.core.next(cljs.core.next(arglist__18777)));
    var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__18777)));
    return update_in__delegate(m, p__18768, f, args)
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
  var this__18780 = this;
  var h__2247__auto____18781 = this__18780.__hash;
  if(!(h__2247__auto____18781 == null)) {
    return h__2247__auto____18781
  }else {
    var h__2247__auto____18782 = cljs.core.hash_coll.call(null, coll);
    this__18780.__hash = h__2247__auto____18782;
    return h__2247__auto____18782
  }
};
cljs.core.Vector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18783 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.Vector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18784 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.Vector.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18785 = this;
  var new_array__18786 = this__18785.array.slice();
  new_array__18786[k] = v;
  return new cljs.core.Vector(this__18785.meta, new_array__18786, null)
};
cljs.core.Vector.prototype.call = function() {
  var G__18817 = null;
  var G__18817__2 = function(this_sym18787, k) {
    var this__18789 = this;
    var this_sym18787__18790 = this;
    var coll__18791 = this_sym18787__18790;
    return coll__18791.cljs$core$ILookup$_lookup$arity$2(coll__18791, k)
  };
  var G__18817__3 = function(this_sym18788, k, not_found) {
    var this__18789 = this;
    var this_sym18788__18792 = this;
    var coll__18793 = this_sym18788__18792;
    return coll__18793.cljs$core$ILookup$_lookup$arity$3(coll__18793, k, not_found)
  };
  G__18817 = function(this_sym18788, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18817__2.call(this, this_sym18788, k);
      case 3:
        return G__18817__3.call(this, this_sym18788, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18817
}();
cljs.core.Vector.prototype.apply = function(this_sym18778, args18779) {
  var this__18794 = this;
  return this_sym18778.call.apply(this_sym18778, [this_sym18778].concat(args18779.slice()))
};
cljs.core.Vector.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18795 = this;
  var new_array__18796 = this__18795.array.slice();
  new_array__18796.push(o);
  return new cljs.core.Vector(this__18795.meta, new_array__18796, null)
};
cljs.core.Vector.prototype.toString = function() {
  var this__18797 = this;
  var this__18798 = this;
  return cljs.core.pr_str.call(null, this__18798)
};
cljs.core.Vector.prototype.cljs$core$IReduce$_reduce$arity$2 = function(v, f) {
  var this__18799 = this;
  return cljs.core.ci_reduce.call(null, this__18799.array, f)
};
cljs.core.Vector.prototype.cljs$core$IReduce$_reduce$arity$3 = function(v, f, start) {
  var this__18800 = this;
  return cljs.core.ci_reduce.call(null, this__18800.array, f, start)
};
cljs.core.Vector.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18801 = this;
  if(this__18801.array.length > 0) {
    var vector_seq__18802 = function vector_seq(i) {
      return new cljs.core.LazySeq(null, false, function() {
        if(i < this__18801.array.length) {
          return cljs.core.cons.call(null, this__18801.array[i], vector_seq.call(null, i + 1))
        }else {
          return null
        }
      }, null)
    };
    return vector_seq__18802.call(null, 0)
  }else {
    return null
  }
};
cljs.core.Vector.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18803 = this;
  return this__18803.array.length
};
cljs.core.Vector.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18804 = this;
  var count__18805 = this__18804.array.length;
  if(count__18805 > 0) {
    return this__18804.array[count__18805 - 1]
  }else {
    return null
  }
};
cljs.core.Vector.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18806 = this;
  if(this__18806.array.length > 0) {
    var new_array__18807 = this__18806.array.slice();
    new_array__18807.pop();
    return new cljs.core.Vector(this__18806.meta, new_array__18807, null)
  }else {
    throw new Error("Can't pop empty vector");
  }
};
cljs.core.Vector.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(coll, n, val) {
  var this__18808 = this;
  return coll.cljs$core$IAssociative$_assoc$arity$3(coll, n, val)
};
cljs.core.Vector.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18809 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.Vector.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18810 = this;
  return new cljs.core.Vector(meta, this__18810.array, this__18810.__hash)
};
cljs.core.Vector.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18811 = this;
  return this__18811.meta
};
cljs.core.Vector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18812 = this;
  if(function() {
    var and__3822__auto____18813 = 0 <= n;
    if(and__3822__auto____18813) {
      return n < this__18812.array.length
    }else {
      return and__3822__auto____18813
    }
  }()) {
    return this__18812.array[n]
  }else {
    return null
  }
};
cljs.core.Vector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18814 = this;
  if(function() {
    var and__3822__auto____18815 = 0 <= n;
    if(and__3822__auto____18815) {
      return n < this__18814.array.length
    }else {
      return and__3822__auto____18815
    }
  }()) {
    return this__18814.array[n]
  }else {
    return not_found
  }
};
cljs.core.Vector.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18816 = this;
  return cljs.core.with_meta.call(null, cljs.core.Vector.EMPTY, this__18816.meta)
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
  var cnt__18819 = pv.cnt;
  if(cnt__18819 < 32) {
    return 0
  }else {
    return cnt__18819 - 1 >>> 5 << 5
  }
};
cljs.core.new_path = function new_path(edit, level, node) {
  var ll__18825 = level;
  var ret__18826 = node;
  while(true) {
    if(ll__18825 === 0) {
      return ret__18826
    }else {
      var embed__18827 = ret__18826;
      var r__18828 = cljs.core.pv_fresh_node.call(null, edit);
      var ___18829 = cljs.core.pv_aset.call(null, r__18828, 0, embed__18827);
      var G__18830 = ll__18825 - 5;
      var G__18831 = r__18828;
      ll__18825 = G__18830;
      ret__18826 = G__18831;
      continue
    }
    break
  }
};
cljs.core.push_tail = function push_tail(pv, level, parent, tailnode) {
  var ret__18837 = cljs.core.pv_clone_node.call(null, parent);
  var subidx__18838 = pv.cnt - 1 >>> level & 31;
  if(5 === level) {
    cljs.core.pv_aset.call(null, ret__18837, subidx__18838, tailnode);
    return ret__18837
  }else {
    var child__18839 = cljs.core.pv_aget.call(null, parent, subidx__18838);
    if(!(child__18839 == null)) {
      var node_to_insert__18840 = push_tail.call(null, pv, level - 5, child__18839, tailnode);
      cljs.core.pv_aset.call(null, ret__18837, subidx__18838, node_to_insert__18840);
      return ret__18837
    }else {
      var node_to_insert__18841 = cljs.core.new_path.call(null, null, level - 5, tailnode);
      cljs.core.pv_aset.call(null, ret__18837, subidx__18838, node_to_insert__18841);
      return ret__18837
    }
  }
};
cljs.core.array_for = function array_for(pv, i) {
  if(function() {
    var and__3822__auto____18845 = 0 <= i;
    if(and__3822__auto____18845) {
      return i < pv.cnt
    }else {
      return and__3822__auto____18845
    }
  }()) {
    if(i >= cljs.core.tail_off.call(null, pv)) {
      return pv.tail
    }else {
      var node__18846 = pv.root;
      var level__18847 = pv.shift;
      while(true) {
        if(level__18847 > 0) {
          var G__18848 = cljs.core.pv_aget.call(null, node__18846, i >>> level__18847 & 31);
          var G__18849 = level__18847 - 5;
          node__18846 = G__18848;
          level__18847 = G__18849;
          continue
        }else {
          return node__18846.arr
        }
        break
      }
    }
  }else {
    throw new Error([cljs.core.str("No item "), cljs.core.str(i), cljs.core.str(" in vector of length "), cljs.core.str(pv.cnt)].join(""));
  }
};
cljs.core.do_assoc = function do_assoc(pv, level, node, i, val) {
  var ret__18852 = cljs.core.pv_clone_node.call(null, node);
  if(level === 0) {
    cljs.core.pv_aset.call(null, ret__18852, i & 31, val);
    return ret__18852
  }else {
    var subidx__18853 = i >>> level & 31;
    cljs.core.pv_aset.call(null, ret__18852, subidx__18853, do_assoc.call(null, pv, level - 5, cljs.core.pv_aget.call(null, node, subidx__18853), i, val));
    return ret__18852
  }
};
cljs.core.pop_tail = function pop_tail(pv, level, node) {
  var subidx__18859 = pv.cnt - 2 >>> level & 31;
  if(level > 5) {
    var new_child__18860 = pop_tail.call(null, pv, level - 5, cljs.core.pv_aget.call(null, node, subidx__18859));
    if(function() {
      var and__3822__auto____18861 = new_child__18860 == null;
      if(and__3822__auto____18861) {
        return subidx__18859 === 0
      }else {
        return and__3822__auto____18861
      }
    }()) {
      return null
    }else {
      var ret__18862 = cljs.core.pv_clone_node.call(null, node);
      cljs.core.pv_aset.call(null, ret__18862, subidx__18859, new_child__18860);
      return ret__18862
    }
  }else {
    if(subidx__18859 === 0) {
      return null
    }else {
      if("\ufdd0'else") {
        var ret__18863 = cljs.core.pv_clone_node.call(null, node);
        cljs.core.pv_aset.call(null, ret__18863, subidx__18859, null);
        return ret__18863
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
  var this__18866 = this;
  return new cljs.core.TransientVector(this__18866.cnt, this__18866.shift, cljs.core.tv_editable_root.call(null, this__18866.root), cljs.core.tv_editable_tail.call(null, this__18866.tail))
};
cljs.core.PersistentVector.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__18867 = this;
  var h__2247__auto____18868 = this__18867.__hash;
  if(!(h__2247__auto____18868 == null)) {
    return h__2247__auto____18868
  }else {
    var h__2247__auto____18869 = cljs.core.hash_coll.call(null, coll);
    this__18867.__hash = h__2247__auto____18869;
    return h__2247__auto____18869
  }
};
cljs.core.PersistentVector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18870 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.PersistentVector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18871 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.PersistentVector.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__18872 = this;
  if(function() {
    var and__3822__auto____18873 = 0 <= k;
    if(and__3822__auto____18873) {
      return k < this__18872.cnt
    }else {
      return and__3822__auto____18873
    }
  }()) {
    if(cljs.core.tail_off.call(null, coll) <= k) {
      var new_tail__18874 = this__18872.tail.slice();
      new_tail__18874[k & 31] = v;
      return new cljs.core.PersistentVector(this__18872.meta, this__18872.cnt, this__18872.shift, this__18872.root, new_tail__18874, null)
    }else {
      return new cljs.core.PersistentVector(this__18872.meta, this__18872.cnt, this__18872.shift, cljs.core.do_assoc.call(null, coll, this__18872.shift, this__18872.root, k, v), this__18872.tail, null)
    }
  }else {
    if(k === this__18872.cnt) {
      return coll.cljs$core$ICollection$_conj$arity$2(coll, v)
    }else {
      if("\ufdd0'else") {
        throw new Error([cljs.core.str("Index "), cljs.core.str(k), cljs.core.str(" out of bounds  [0,"), cljs.core.str(this__18872.cnt), cljs.core.str("]")].join(""));
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentVector.prototype.call = function() {
  var G__18922 = null;
  var G__18922__2 = function(this_sym18875, k) {
    var this__18877 = this;
    var this_sym18875__18878 = this;
    var coll__18879 = this_sym18875__18878;
    return coll__18879.cljs$core$ILookup$_lookup$arity$2(coll__18879, k)
  };
  var G__18922__3 = function(this_sym18876, k, not_found) {
    var this__18877 = this;
    var this_sym18876__18880 = this;
    var coll__18881 = this_sym18876__18880;
    return coll__18881.cljs$core$ILookup$_lookup$arity$3(coll__18881, k, not_found)
  };
  G__18922 = function(this_sym18876, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18922__2.call(this, this_sym18876, k);
      case 3:
        return G__18922__3.call(this, this_sym18876, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18922
}();
cljs.core.PersistentVector.prototype.apply = function(this_sym18864, args18865) {
  var this__18882 = this;
  return this_sym18864.call.apply(this_sym18864, [this_sym18864].concat(args18865.slice()))
};
cljs.core.PersistentVector.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(v, f, init) {
  var this__18883 = this;
  var step_init__18884 = [0, init];
  var i__18885 = 0;
  while(true) {
    if(i__18885 < this__18883.cnt) {
      var arr__18886 = cljs.core.array_for.call(null, v, i__18885);
      var len__18887 = arr__18886.length;
      var init__18891 = function() {
        var j__18888 = 0;
        var init__18889 = step_init__18884[1];
        while(true) {
          if(j__18888 < len__18887) {
            var init__18890 = f.call(null, init__18889, j__18888 + i__18885, arr__18886[j__18888]);
            if(cljs.core.reduced_QMARK_.call(null, init__18890)) {
              return init__18890
            }else {
              var G__18923 = j__18888 + 1;
              var G__18924 = init__18890;
              j__18888 = G__18923;
              init__18889 = G__18924;
              continue
            }
          }else {
            step_init__18884[0] = len__18887;
            step_init__18884[1] = init__18889;
            return init__18889
          }
          break
        }
      }();
      if(cljs.core.reduced_QMARK_.call(null, init__18891)) {
        return cljs.core.deref.call(null, init__18891)
      }else {
        var G__18925 = i__18885 + step_init__18884[0];
        i__18885 = G__18925;
        continue
      }
    }else {
      return step_init__18884[1]
    }
    break
  }
};
cljs.core.PersistentVector.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18892 = this;
  if(this__18892.cnt - cljs.core.tail_off.call(null, coll) < 32) {
    var new_tail__18893 = this__18892.tail.slice();
    new_tail__18893.push(o);
    return new cljs.core.PersistentVector(this__18892.meta, this__18892.cnt + 1, this__18892.shift, this__18892.root, new_tail__18893, null)
  }else {
    var root_overflow_QMARK___18894 = this__18892.cnt >>> 5 > 1 << this__18892.shift;
    var new_shift__18895 = root_overflow_QMARK___18894 ? this__18892.shift + 5 : this__18892.shift;
    var new_root__18897 = root_overflow_QMARK___18894 ? function() {
      var n_r__18896 = cljs.core.pv_fresh_node.call(null, null);
      cljs.core.pv_aset.call(null, n_r__18896, 0, this__18892.root);
      cljs.core.pv_aset.call(null, n_r__18896, 1, cljs.core.new_path.call(null, null, this__18892.shift, new cljs.core.VectorNode(null, this__18892.tail)));
      return n_r__18896
    }() : cljs.core.push_tail.call(null, coll, this__18892.shift, this__18892.root, new cljs.core.VectorNode(null, this__18892.tail));
    return new cljs.core.PersistentVector(this__18892.meta, this__18892.cnt + 1, new_shift__18895, new_root__18897, [o], null)
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__18898 = this;
  if(this__18898.cnt > 0) {
    return new cljs.core.RSeq(coll, this__18898.cnt - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IMapEntry$_key$arity$1 = function(coll) {
  var this__18899 = this;
  return coll.cljs$core$IIndexed$_nth$arity$2(coll, 0)
};
cljs.core.PersistentVector.prototype.cljs$core$IMapEntry$_val$arity$1 = function(coll) {
  var this__18900 = this;
  return coll.cljs$core$IIndexed$_nth$arity$2(coll, 1)
};
cljs.core.PersistentVector.prototype.toString = function() {
  var this__18901 = this;
  var this__18902 = this;
  return cljs.core.pr_str.call(null, this__18902)
};
cljs.core.PersistentVector.prototype.cljs$core$IReduce$_reduce$arity$2 = function(v, f) {
  var this__18903 = this;
  return cljs.core.ci_reduce.call(null, v, f)
};
cljs.core.PersistentVector.prototype.cljs$core$IReduce$_reduce$arity$3 = function(v, f, start) {
  var this__18904 = this;
  return cljs.core.ci_reduce.call(null, v, f, start)
};
cljs.core.PersistentVector.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18905 = this;
  if(this__18905.cnt === 0) {
    return null
  }else {
    return cljs.core.chunked_seq.call(null, coll, 0, 0)
  }
};
cljs.core.PersistentVector.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18906 = this;
  return this__18906.cnt
};
cljs.core.PersistentVector.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18907 = this;
  if(this__18907.cnt > 0) {
    return coll.cljs$core$IIndexed$_nth$arity$2(coll, this__18907.cnt - 1)
  }else {
    return null
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18908 = this;
  if(this__18908.cnt === 0) {
    throw new Error("Can't pop empty vector");
  }else {
    if(1 === this__18908.cnt) {
      return cljs.core._with_meta.call(null, cljs.core.PersistentVector.EMPTY, this__18908.meta)
    }else {
      if(1 < this__18908.cnt - cljs.core.tail_off.call(null, coll)) {
        return new cljs.core.PersistentVector(this__18908.meta, this__18908.cnt - 1, this__18908.shift, this__18908.root, this__18908.tail.slice(0, -1), null)
      }else {
        if("\ufdd0'else") {
          var new_tail__18909 = cljs.core.array_for.call(null, coll, this__18908.cnt - 2);
          var nr__18910 = cljs.core.pop_tail.call(null, coll, this__18908.shift, this__18908.root);
          var new_root__18911 = nr__18910 == null ? cljs.core.PersistentVector.EMPTY_NODE : nr__18910;
          var cnt_1__18912 = this__18908.cnt - 1;
          if(function() {
            var and__3822__auto____18913 = 5 < this__18908.shift;
            if(and__3822__auto____18913) {
              return cljs.core.pv_aget.call(null, new_root__18911, 1) == null
            }else {
              return and__3822__auto____18913
            }
          }()) {
            return new cljs.core.PersistentVector(this__18908.meta, cnt_1__18912, this__18908.shift - 5, cljs.core.pv_aget.call(null, new_root__18911, 0), new_tail__18909, null)
          }else {
            return new cljs.core.PersistentVector(this__18908.meta, cnt_1__18912, this__18908.shift, new_root__18911, new_tail__18909, null)
          }
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(coll, n, val) {
  var this__18914 = this;
  return coll.cljs$core$IAssociative$_assoc$arity$3(coll, n, val)
};
cljs.core.PersistentVector.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18915 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentVector.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18916 = this;
  return new cljs.core.PersistentVector(meta, this__18916.cnt, this__18916.shift, this__18916.root, this__18916.tail, this__18916.__hash)
};
cljs.core.PersistentVector.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18917 = this;
  return this__18917.meta
};
cljs.core.PersistentVector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18918 = this;
  return cljs.core.array_for.call(null, coll, n)[n & 31]
};
cljs.core.PersistentVector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18919 = this;
  if(function() {
    var and__3822__auto____18920 = 0 <= n;
    if(and__3822__auto____18920) {
      return n < this__18919.cnt
    }else {
      return and__3822__auto____18920
    }
  }()) {
    return coll.cljs$core$IIndexed$_nth$arity$2(coll, n)
  }else {
    return not_found
  }
};
cljs.core.PersistentVector.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18921 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.EMPTY, this__18921.meta)
};
cljs.core.PersistentVector;
cljs.core.PersistentVector.EMPTY_NODE = cljs.core.pv_fresh_node.call(null, null);
cljs.core.PersistentVector.EMPTY = new cljs.core.PersistentVector(null, 0, 5, cljs.core.PersistentVector.EMPTY_NODE, [], 0);
cljs.core.PersistentVector.fromArray = function(xs, no_clone) {
  var l__18926 = xs.length;
  var xs__18927 = no_clone === true ? xs : xs.slice();
  if(l__18926 < 32) {
    return new cljs.core.PersistentVector(null, l__18926, 5, cljs.core.PersistentVector.EMPTY_NODE, xs__18927, null)
  }else {
    var node__18928 = xs__18927.slice(0, 32);
    var v__18929 = new cljs.core.PersistentVector(null, 32, 5, cljs.core.PersistentVector.EMPTY_NODE, node__18928, null);
    var i__18930 = 32;
    var out__18931 = cljs.core._as_transient.call(null, v__18929);
    while(true) {
      if(i__18930 < l__18926) {
        var G__18932 = i__18930 + 1;
        var G__18933 = cljs.core.conj_BANG_.call(null, out__18931, xs__18927[i__18930]);
        i__18930 = G__18932;
        out__18931 = G__18933;
        continue
      }else {
        return cljs.core.persistent_BANG_.call(null, out__18931)
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
  vector.cljs$lang$applyTo = function(arglist__18934) {
    var args = cljs.core.seq(arglist__18934);
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
  var this__18935 = this;
  var h__2247__auto____18936 = this__18935.__hash;
  if(!(h__2247__auto____18936 == null)) {
    return h__2247__auto____18936
  }else {
    var h__2247__auto____18937 = cljs.core.hash_coll.call(null, coll);
    this__18935.__hash = h__2247__auto____18937;
    return h__2247__auto____18937
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$INext$_next$arity$1 = function(coll) {
  var this__18938 = this;
  if(this__18938.off + 1 < this__18938.node.length) {
    var s__18939 = cljs.core.chunked_seq.call(null, this__18938.vec, this__18938.node, this__18938.i, this__18938.off + 1);
    if(s__18939 == null) {
      return null
    }else {
      return s__18939
    }
  }else {
    return coll.cljs$core$IChunkedNext$_chunked_next$arity$1(coll)
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18940 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18941 = this;
  return coll
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__18942 = this;
  return this__18942.node[this__18942.off]
};
cljs.core.ChunkedSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__18943 = this;
  if(this__18943.off + 1 < this__18943.node.length) {
    var s__18944 = cljs.core.chunked_seq.call(null, this__18943.vec, this__18943.node, this__18943.i, this__18943.off + 1);
    if(s__18944 == null) {
      return cljs.core.List.EMPTY
    }else {
      return s__18944
    }
  }else {
    return coll.cljs$core$IChunkedSeq$_chunked_rest$arity$1(coll)
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedNext$_chunked_next$arity$1 = function(coll) {
  var this__18945 = this;
  var l__18946 = this__18945.node.length;
  var s__18947 = this__18945.i + l__18946 < cljs.core._count.call(null, this__18945.vec) ? cljs.core.chunked_seq.call(null, this__18945.vec, this__18945.i + l__18946, 0) : null;
  if(s__18947 == null) {
    return null
  }else {
    return s__18947
  }
};
cljs.core.ChunkedSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18948 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, m) {
  var this__18949 = this;
  return cljs.core.chunked_seq.call(null, this__18949.vec, this__18949.node, this__18949.i, this__18949.off, m)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IWithMeta$_meta$arity$1 = function(coll) {
  var this__18950 = this;
  return this__18950.meta
};
cljs.core.ChunkedSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18951 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.EMPTY, this__18951.meta)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedSeq$_chunked_first$arity$1 = function(coll) {
  var this__18952 = this;
  return cljs.core.array_chunk.call(null, this__18952.node, this__18952.off)
};
cljs.core.ChunkedSeq.prototype.cljs$core$IChunkedSeq$_chunked_rest$arity$1 = function(coll) {
  var this__18953 = this;
  var l__18954 = this__18953.node.length;
  var s__18955 = this__18953.i + l__18954 < cljs.core._count.call(null, this__18953.vec) ? cljs.core.chunked_seq.call(null, this__18953.vec, this__18953.i + l__18954, 0) : null;
  if(s__18955 == null) {
    return cljs.core.List.EMPTY
  }else {
    return s__18955
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
  var this__18958 = this;
  var h__2247__auto____18959 = this__18958.__hash;
  if(!(h__2247__auto____18959 == null)) {
    return h__2247__auto____18959
  }else {
    var h__2247__auto____18960 = cljs.core.hash_coll.call(null, coll);
    this__18958.__hash = h__2247__auto____18960;
    return h__2247__auto____18960
  }
};
cljs.core.Subvec.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__18961 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.Subvec.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__18962 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.Subvec.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, key, val) {
  var this__18963 = this;
  var v_pos__18964 = this__18963.start + key;
  return new cljs.core.Subvec(this__18963.meta, cljs.core._assoc.call(null, this__18963.v, v_pos__18964, val), this__18963.start, this__18963.end > v_pos__18964 + 1 ? this__18963.end : v_pos__18964 + 1, null)
};
cljs.core.Subvec.prototype.call = function() {
  var G__18990 = null;
  var G__18990__2 = function(this_sym18965, k) {
    var this__18967 = this;
    var this_sym18965__18968 = this;
    var coll__18969 = this_sym18965__18968;
    return coll__18969.cljs$core$ILookup$_lookup$arity$2(coll__18969, k)
  };
  var G__18990__3 = function(this_sym18966, k, not_found) {
    var this__18967 = this;
    var this_sym18966__18970 = this;
    var coll__18971 = this_sym18966__18970;
    return coll__18971.cljs$core$ILookup$_lookup$arity$3(coll__18971, k, not_found)
  };
  G__18990 = function(this_sym18966, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__18990__2.call(this, this_sym18966, k);
      case 3:
        return G__18990__3.call(this, this_sym18966, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__18990
}();
cljs.core.Subvec.prototype.apply = function(this_sym18956, args18957) {
  var this__18972 = this;
  return this_sym18956.call.apply(this_sym18956, [this_sym18956].concat(args18957.slice()))
};
cljs.core.Subvec.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__18973 = this;
  return new cljs.core.Subvec(this__18973.meta, cljs.core._assoc_n.call(null, this__18973.v, this__18973.end, o), this__18973.start, this__18973.end + 1, null)
};
cljs.core.Subvec.prototype.toString = function() {
  var this__18974 = this;
  var this__18975 = this;
  return cljs.core.pr_str.call(null, this__18975)
};
cljs.core.Subvec.prototype.cljs$core$IReduce$_reduce$arity$2 = function(coll, f) {
  var this__18976 = this;
  return cljs.core.ci_reduce.call(null, coll, f)
};
cljs.core.Subvec.prototype.cljs$core$IReduce$_reduce$arity$3 = function(coll, f, start) {
  var this__18977 = this;
  return cljs.core.ci_reduce.call(null, coll, f, start)
};
cljs.core.Subvec.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__18978 = this;
  var subvec_seq__18979 = function subvec_seq(i) {
    if(i === this__18978.end) {
      return null
    }else {
      return cljs.core.cons.call(null, cljs.core._nth.call(null, this__18978.v, i), new cljs.core.LazySeq(null, false, function() {
        return subvec_seq.call(null, i + 1)
      }, null))
    }
  };
  return subvec_seq__18979.call(null, this__18978.start)
};
cljs.core.Subvec.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__18980 = this;
  return this__18980.end - this__18980.start
};
cljs.core.Subvec.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__18981 = this;
  return cljs.core._nth.call(null, this__18981.v, this__18981.end - 1)
};
cljs.core.Subvec.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__18982 = this;
  if(this__18982.start === this__18982.end) {
    throw new Error("Can't pop empty vector");
  }else {
    return new cljs.core.Subvec(this__18982.meta, this__18982.v, this__18982.start, this__18982.end - 1, null)
  }
};
cljs.core.Subvec.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(coll, n, val) {
  var this__18983 = this;
  return coll.cljs$core$IAssociative$_assoc$arity$3(coll, n, val)
};
cljs.core.Subvec.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__18984 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.Subvec.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__18985 = this;
  return new cljs.core.Subvec(meta, this__18985.v, this__18985.start, this__18985.end, this__18985.__hash)
};
cljs.core.Subvec.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__18986 = this;
  return this__18986.meta
};
cljs.core.Subvec.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__18987 = this;
  return cljs.core._nth.call(null, this__18987.v, this__18987.start + n)
};
cljs.core.Subvec.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__18988 = this;
  return cljs.core._nth.call(null, this__18988.v, this__18988.start + n, not_found)
};
cljs.core.Subvec.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__18989 = this;
  return cljs.core.with_meta.call(null, cljs.core.Vector.EMPTY, this__18989.meta)
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
  var ret__18992 = cljs.core.make_array.call(null, 32);
  cljs.core.array_copy.call(null, tl, 0, ret__18992, 0, tl.length);
  return ret__18992
};
cljs.core.tv_push_tail = function tv_push_tail(tv, level, parent, tail_node) {
  var ret__18996 = cljs.core.tv_ensure_editable.call(null, tv.root.edit, parent);
  var subidx__18997 = tv.cnt - 1 >>> level & 31;
  cljs.core.pv_aset.call(null, ret__18996, subidx__18997, level === 5 ? tail_node : function() {
    var child__18998 = cljs.core.pv_aget.call(null, ret__18996, subidx__18997);
    if(!(child__18998 == null)) {
      return tv_push_tail.call(null, tv, level - 5, child__18998, tail_node)
    }else {
      return cljs.core.new_path.call(null, tv.root.edit, level - 5, tail_node)
    }
  }());
  return ret__18996
};
cljs.core.tv_pop_tail = function tv_pop_tail(tv, level, node) {
  var node__19003 = cljs.core.tv_ensure_editable.call(null, tv.root.edit, node);
  var subidx__19004 = tv.cnt - 2 >>> level & 31;
  if(level > 5) {
    var new_child__19005 = tv_pop_tail.call(null, tv, level - 5, cljs.core.pv_aget.call(null, node__19003, subidx__19004));
    if(function() {
      var and__3822__auto____19006 = new_child__19005 == null;
      if(and__3822__auto____19006) {
        return subidx__19004 === 0
      }else {
        return and__3822__auto____19006
      }
    }()) {
      return null
    }else {
      cljs.core.pv_aset.call(null, node__19003, subidx__19004, new_child__19005);
      return node__19003
    }
  }else {
    if(subidx__19004 === 0) {
      return null
    }else {
      if("\ufdd0'else") {
        cljs.core.pv_aset.call(null, node__19003, subidx__19004, null);
        return node__19003
      }else {
        return null
      }
    }
  }
};
cljs.core.editable_array_for = function editable_array_for(tv, i) {
  if(function() {
    var and__3822__auto____19011 = 0 <= i;
    if(and__3822__auto____19011) {
      return i < tv.cnt
    }else {
      return and__3822__auto____19011
    }
  }()) {
    if(i >= cljs.core.tail_off.call(null, tv)) {
      return tv.tail
    }else {
      var root__19012 = tv.root;
      var node__19013 = root__19012;
      var level__19014 = tv.shift;
      while(true) {
        if(level__19014 > 0) {
          var G__19015 = cljs.core.tv_ensure_editable.call(null, root__19012.edit, cljs.core.pv_aget.call(null, node__19013, i >>> level__19014 & 31));
          var G__19016 = level__19014 - 5;
          node__19013 = G__19015;
          level__19014 = G__19016;
          continue
        }else {
          return node__19013.arr
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
  var G__19056 = null;
  var G__19056__2 = function(this_sym19019, k) {
    var this__19021 = this;
    var this_sym19019__19022 = this;
    var coll__19023 = this_sym19019__19022;
    return coll__19023.cljs$core$ILookup$_lookup$arity$2(coll__19023, k)
  };
  var G__19056__3 = function(this_sym19020, k, not_found) {
    var this__19021 = this;
    var this_sym19020__19024 = this;
    var coll__19025 = this_sym19020__19024;
    return coll__19025.cljs$core$ILookup$_lookup$arity$3(coll__19025, k, not_found)
  };
  G__19056 = function(this_sym19020, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19056__2.call(this, this_sym19020, k);
      case 3:
        return G__19056__3.call(this, this_sym19020, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19056
}();
cljs.core.TransientVector.prototype.apply = function(this_sym19017, args19018) {
  var this__19026 = this;
  return this_sym19017.call.apply(this_sym19017, [this_sym19017].concat(args19018.slice()))
};
cljs.core.TransientVector.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19027 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, null)
};
cljs.core.TransientVector.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19028 = this;
  return coll.cljs$core$IIndexed$_nth$arity$3(coll, k, not_found)
};
cljs.core.TransientVector.prototype.cljs$core$IIndexed$_nth$arity$2 = function(coll, n) {
  var this__19029 = this;
  if(this__19029.root.edit) {
    return cljs.core.array_for.call(null, coll, n)[n & 31]
  }else {
    throw new Error("nth after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$IIndexed$_nth$arity$3 = function(coll, n, not_found) {
  var this__19030 = this;
  if(function() {
    var and__3822__auto____19031 = 0 <= n;
    if(and__3822__auto____19031) {
      return n < this__19030.cnt
    }else {
      return and__3822__auto____19031
    }
  }()) {
    return coll.cljs$core$IIndexed$_nth$arity$2(coll, n)
  }else {
    return not_found
  }
};
cljs.core.TransientVector.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19032 = this;
  if(this__19032.root.edit) {
    return this__19032.cnt
  }else {
    throw new Error("count after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3 = function(tcoll, n, val) {
  var this__19033 = this;
  if(this__19033.root.edit) {
    if(function() {
      var and__3822__auto____19034 = 0 <= n;
      if(and__3822__auto____19034) {
        return n < this__19033.cnt
      }else {
        return and__3822__auto____19034
      }
    }()) {
      if(cljs.core.tail_off.call(null, tcoll) <= n) {
        this__19033.tail[n & 31] = val;
        return tcoll
      }else {
        var new_root__19039 = function go(level, node) {
          var node__19037 = cljs.core.tv_ensure_editable.call(null, this__19033.root.edit, node);
          if(level === 0) {
            cljs.core.pv_aset.call(null, node__19037, n & 31, val);
            return node__19037
          }else {
            var subidx__19038 = n >>> level & 31;
            cljs.core.pv_aset.call(null, node__19037, subidx__19038, go.call(null, level - 5, cljs.core.pv_aget.call(null, node__19037, subidx__19038)));
            return node__19037
          }
        }.call(null, this__19033.shift, this__19033.root);
        this__19033.root = new_root__19039;
        return tcoll
      }
    }else {
      if(n === this__19033.cnt) {
        return tcoll.cljs$core$ITransientCollection$_conj_BANG_$arity$2(tcoll, val)
      }else {
        if("\ufdd0'else") {
          throw new Error([cljs.core.str("Index "), cljs.core.str(n), cljs.core.str(" out of bounds for TransientVector of length"), cljs.core.str(this__19033.cnt)].join(""));
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
  var this__19040 = this;
  if(this__19040.root.edit) {
    if(this__19040.cnt === 0) {
      throw new Error("Can't pop empty vector");
    }else {
      if(1 === this__19040.cnt) {
        this__19040.cnt = 0;
        return tcoll
      }else {
        if((this__19040.cnt - 1 & 31) > 0) {
          this__19040.cnt = this__19040.cnt - 1;
          return tcoll
        }else {
          if("\ufdd0'else") {
            var new_tail__19041 = cljs.core.editable_array_for.call(null, tcoll, this__19040.cnt - 2);
            var new_root__19043 = function() {
              var nr__19042 = cljs.core.tv_pop_tail.call(null, tcoll, this__19040.shift, this__19040.root);
              if(!(nr__19042 == null)) {
                return nr__19042
              }else {
                return new cljs.core.VectorNode(this__19040.root.edit, cljs.core.make_array.call(null, 32))
              }
            }();
            if(function() {
              var and__3822__auto____19044 = 5 < this__19040.shift;
              if(and__3822__auto____19044) {
                return cljs.core.pv_aget.call(null, new_root__19043, 1) == null
              }else {
                return and__3822__auto____19044
              }
            }()) {
              var new_root__19045 = cljs.core.tv_ensure_editable.call(null, this__19040.root.edit, cljs.core.pv_aget.call(null, new_root__19043, 0));
              this__19040.root = new_root__19045;
              this__19040.shift = this__19040.shift - 5;
              this__19040.cnt = this__19040.cnt - 1;
              this__19040.tail = new_tail__19041;
              return tcoll
            }else {
              this__19040.root = new_root__19043;
              this__19040.cnt = this__19040.cnt - 1;
              this__19040.tail = new_tail__19041;
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
  var this__19046 = this;
  return tcoll.cljs$core$ITransientVector$_assoc_n_BANG_$arity$3(tcoll, key, val)
};
cljs.core.TransientVector.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, o) {
  var this__19047 = this;
  if(this__19047.root.edit) {
    if(this__19047.cnt - cljs.core.tail_off.call(null, tcoll) < 32) {
      this__19047.tail[this__19047.cnt & 31] = o;
      this__19047.cnt = this__19047.cnt + 1;
      return tcoll
    }else {
      var tail_node__19048 = new cljs.core.VectorNode(this__19047.root.edit, this__19047.tail);
      var new_tail__19049 = cljs.core.make_array.call(null, 32);
      new_tail__19049[0] = o;
      this__19047.tail = new_tail__19049;
      if(this__19047.cnt >>> 5 > 1 << this__19047.shift) {
        var new_root_array__19050 = cljs.core.make_array.call(null, 32);
        var new_shift__19051 = this__19047.shift + 5;
        new_root_array__19050[0] = this__19047.root;
        new_root_array__19050[1] = cljs.core.new_path.call(null, this__19047.root.edit, this__19047.shift, tail_node__19048);
        this__19047.root = new cljs.core.VectorNode(this__19047.root.edit, new_root_array__19050);
        this__19047.shift = new_shift__19051;
        this__19047.cnt = this__19047.cnt + 1;
        return tcoll
      }else {
        var new_root__19052 = cljs.core.tv_push_tail.call(null, tcoll, this__19047.shift, this__19047.root, tail_node__19048);
        this__19047.root = new_root__19052;
        this__19047.cnt = this__19047.cnt + 1;
        return tcoll
      }
    }
  }else {
    throw new Error("conj! after persistent!");
  }
};
cljs.core.TransientVector.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__19053 = this;
  if(this__19053.root.edit) {
    this__19053.root.edit = null;
    var len__19054 = this__19053.cnt - cljs.core.tail_off.call(null, tcoll);
    var trimmed_tail__19055 = cljs.core.make_array.call(null, len__19054);
    cljs.core.array_copy.call(null, this__19053.tail, 0, trimmed_tail__19055, 0, len__19054);
    return new cljs.core.PersistentVector(null, this__19053.cnt, this__19053.shift, this__19053.root, trimmed_tail__19055, null)
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
  var this__19057 = this;
  var h__2247__auto____19058 = this__19057.__hash;
  if(!(h__2247__auto____19058 == null)) {
    return h__2247__auto____19058
  }else {
    var h__2247__auto____19059 = cljs.core.hash_coll.call(null, coll);
    this__19057.__hash = h__2247__auto____19059;
    return h__2247__auto____19059
  }
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19060 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.PersistentQueueSeq.prototype.toString = function() {
  var this__19061 = this;
  var this__19062 = this;
  return cljs.core.pr_str.call(null, this__19062)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19063 = this;
  return coll
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__19064 = this;
  return cljs.core._first.call(null, this__19064.front)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__19065 = this;
  var temp__3971__auto____19066 = cljs.core.next.call(null, this__19065.front);
  if(temp__3971__auto____19066) {
    var f1__19067 = temp__3971__auto____19066;
    return new cljs.core.PersistentQueueSeq(this__19065.meta, f1__19067, this__19065.rear, null)
  }else {
    if(this__19065.rear == null) {
      return coll.cljs$core$IEmptyableCollection$_empty$arity$1(coll)
    }else {
      return new cljs.core.PersistentQueueSeq(this__19065.meta, this__19065.rear, null, null)
    }
  }
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19068 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19069 = this;
  return new cljs.core.PersistentQueueSeq(meta, this__19069.front, this__19069.rear, this__19069.__hash)
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19070 = this;
  return this__19070.meta
};
cljs.core.PersistentQueueSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19071 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19071.meta)
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
  var this__19072 = this;
  var h__2247__auto____19073 = this__19072.__hash;
  if(!(h__2247__auto____19073 == null)) {
    return h__2247__auto____19073
  }else {
    var h__2247__auto____19074 = cljs.core.hash_coll.call(null, coll);
    this__19072.__hash = h__2247__auto____19074;
    return h__2247__auto____19074
  }
};
cljs.core.PersistentQueue.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19075 = this;
  if(cljs.core.truth_(this__19075.front)) {
    return new cljs.core.PersistentQueue(this__19075.meta, this__19075.count + 1, this__19075.front, cljs.core.conj.call(null, function() {
      var or__3824__auto____19076 = this__19075.rear;
      if(cljs.core.truth_(or__3824__auto____19076)) {
        return or__3824__auto____19076
      }else {
        return cljs.core.PersistentVector.EMPTY
      }
    }(), o), null)
  }else {
    return new cljs.core.PersistentQueue(this__19075.meta, this__19075.count + 1, cljs.core.conj.call(null, this__19075.front, o), cljs.core.PersistentVector.EMPTY, null)
  }
};
cljs.core.PersistentQueue.prototype.toString = function() {
  var this__19077 = this;
  var this__19078 = this;
  return cljs.core.pr_str.call(null, this__19078)
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19079 = this;
  var rear__19080 = cljs.core.seq.call(null, this__19079.rear);
  if(cljs.core.truth_(function() {
    var or__3824__auto____19081 = this__19079.front;
    if(cljs.core.truth_(or__3824__auto____19081)) {
      return or__3824__auto____19081
    }else {
      return rear__19080
    }
  }())) {
    return new cljs.core.PersistentQueueSeq(null, this__19079.front, cljs.core.seq.call(null, rear__19080), null)
  }else {
    return null
  }
};
cljs.core.PersistentQueue.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19082 = this;
  return this__19082.count
};
cljs.core.PersistentQueue.prototype.cljs$core$IStack$_peek$arity$1 = function(coll) {
  var this__19083 = this;
  return cljs.core._first.call(null, this__19083.front)
};
cljs.core.PersistentQueue.prototype.cljs$core$IStack$_pop$arity$1 = function(coll) {
  var this__19084 = this;
  if(cljs.core.truth_(this__19084.front)) {
    var temp__3971__auto____19085 = cljs.core.next.call(null, this__19084.front);
    if(temp__3971__auto____19085) {
      var f1__19086 = temp__3971__auto____19085;
      return new cljs.core.PersistentQueue(this__19084.meta, this__19084.count - 1, f1__19086, this__19084.rear, null)
    }else {
      return new cljs.core.PersistentQueue(this__19084.meta, this__19084.count - 1, cljs.core.seq.call(null, this__19084.rear), cljs.core.PersistentVector.EMPTY, null)
    }
  }else {
    return coll
  }
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__19087 = this;
  return cljs.core.first.call(null, this__19087.front)
};
cljs.core.PersistentQueue.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__19088 = this;
  return cljs.core.rest.call(null, cljs.core.seq.call(null, coll))
};
cljs.core.PersistentQueue.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19089 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentQueue.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19090 = this;
  return new cljs.core.PersistentQueue(meta, this__19090.count, this__19090.front, this__19090.rear, this__19090.__hash)
};
cljs.core.PersistentQueue.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19091 = this;
  return this__19091.meta
};
cljs.core.PersistentQueue.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19092 = this;
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
  var this__19093 = this;
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
  var len__19096 = array.length;
  var i__19097 = 0;
  while(true) {
    if(i__19097 < len__19096) {
      if(k === array[i__19097]) {
        return i__19097
      }else {
        var G__19098 = i__19097 + incr;
        i__19097 = G__19098;
        continue
      }
    }else {
      return null
    }
    break
  }
};
cljs.core.obj_map_compare_keys = function obj_map_compare_keys(a, b) {
  var a__19101 = cljs.core.hash.call(null, a);
  var b__19102 = cljs.core.hash.call(null, b);
  if(a__19101 < b__19102) {
    return-1
  }else {
    if(a__19101 > b__19102) {
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
  var ks__19110 = m.keys;
  var len__19111 = ks__19110.length;
  var so__19112 = m.strobj;
  var out__19113 = cljs.core.with_meta.call(null, cljs.core.PersistentHashMap.EMPTY, cljs.core.meta.call(null, m));
  var i__19114 = 0;
  var out__19115 = cljs.core.transient$.call(null, out__19113);
  while(true) {
    if(i__19114 < len__19111) {
      var k__19116 = ks__19110[i__19114];
      var G__19117 = i__19114 + 1;
      var G__19118 = cljs.core.assoc_BANG_.call(null, out__19115, k__19116, so__19112[k__19116]);
      i__19114 = G__19117;
      out__19115 = G__19118;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, cljs.core.assoc_BANG_.call(null, out__19115, k, v))
    }
    break
  }
};
cljs.core.obj_clone = function obj_clone(obj, ks) {
  var new_obj__19124 = {};
  var l__19125 = ks.length;
  var i__19126 = 0;
  while(true) {
    if(i__19126 < l__19125) {
      var k__19127 = ks[i__19126];
      new_obj__19124[k__19127] = obj[k__19127];
      var G__19128 = i__19126 + 1;
      i__19126 = G__19128;
      continue
    }else {
    }
    break
  }
  return new_obj__19124
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
  var this__19131 = this;
  return cljs.core.transient$.call(null, cljs.core.into.call(null, cljs.core.hash_map.call(null), coll))
};
cljs.core.ObjMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19132 = this;
  var h__2247__auto____19133 = this__19132.__hash;
  if(!(h__2247__auto____19133 == null)) {
    return h__2247__auto____19133
  }else {
    var h__2247__auto____19134 = cljs.core.hash_imap.call(null, coll);
    this__19132.__hash = h__2247__auto____19134;
    return h__2247__auto____19134
  }
};
cljs.core.ObjMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19135 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.ObjMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19136 = this;
  if(function() {
    var and__3822__auto____19137 = goog.isString(k);
    if(and__3822__auto____19137) {
      return!(cljs.core.scan_array.call(null, 1, k, this__19136.keys) == null)
    }else {
      return and__3822__auto____19137
    }
  }()) {
    return this__19136.strobj[k]
  }else {
    return not_found
  }
};
cljs.core.ObjMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19138 = this;
  if(goog.isString(k)) {
    if(function() {
      var or__3824__auto____19139 = this__19138.update_count > cljs.core.ObjMap.HASHMAP_THRESHOLD;
      if(or__3824__auto____19139) {
        return or__3824__auto____19139
      }else {
        return this__19138.keys.length >= cljs.core.ObjMap.HASHMAP_THRESHOLD
      }
    }()) {
      return cljs.core.obj_map__GT_hash_map.call(null, coll, k, v)
    }else {
      if(!(cljs.core.scan_array.call(null, 1, k, this__19138.keys) == null)) {
        var new_strobj__19140 = cljs.core.obj_clone.call(null, this__19138.strobj, this__19138.keys);
        new_strobj__19140[k] = v;
        return new cljs.core.ObjMap(this__19138.meta, this__19138.keys, new_strobj__19140, this__19138.update_count + 1, null)
      }else {
        var new_strobj__19141 = cljs.core.obj_clone.call(null, this__19138.strobj, this__19138.keys);
        var new_keys__19142 = this__19138.keys.slice();
        new_strobj__19141[k] = v;
        new_keys__19142.push(k);
        return new cljs.core.ObjMap(this__19138.meta, new_keys__19142, new_strobj__19141, this__19138.update_count + 1, null)
      }
    }
  }else {
    return cljs.core.obj_map__GT_hash_map.call(null, coll, k, v)
  }
};
cljs.core.ObjMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19143 = this;
  if(function() {
    var and__3822__auto____19144 = goog.isString(k);
    if(and__3822__auto____19144) {
      return!(cljs.core.scan_array.call(null, 1, k, this__19143.keys) == null)
    }else {
      return and__3822__auto____19144
    }
  }()) {
    return true
  }else {
    return false
  }
};
cljs.core.ObjMap.prototype.call = function() {
  var G__19166 = null;
  var G__19166__2 = function(this_sym19145, k) {
    var this__19147 = this;
    var this_sym19145__19148 = this;
    var coll__19149 = this_sym19145__19148;
    return coll__19149.cljs$core$ILookup$_lookup$arity$2(coll__19149, k)
  };
  var G__19166__3 = function(this_sym19146, k, not_found) {
    var this__19147 = this;
    var this_sym19146__19150 = this;
    var coll__19151 = this_sym19146__19150;
    return coll__19151.cljs$core$ILookup$_lookup$arity$3(coll__19151, k, not_found)
  };
  G__19166 = function(this_sym19146, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19166__2.call(this, this_sym19146, k);
      case 3:
        return G__19166__3.call(this, this_sym19146, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19166
}();
cljs.core.ObjMap.prototype.apply = function(this_sym19129, args19130) {
  var this__19152 = this;
  return this_sym19129.call.apply(this_sym19129, [this_sym19129].concat(args19130.slice()))
};
cljs.core.ObjMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19153 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.ObjMap.prototype.toString = function() {
  var this__19154 = this;
  var this__19155 = this;
  return cljs.core.pr_str.call(null, this__19155)
};
cljs.core.ObjMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19156 = this;
  if(this__19156.keys.length > 0) {
    return cljs.core.map.call(null, function(p1__19119_SHARP_) {
      return cljs.core.vector.call(null, p1__19119_SHARP_, this__19156.strobj[p1__19119_SHARP_])
    }, this__19156.keys.sort(cljs.core.obj_map_compare_keys))
  }else {
    return null
  }
};
cljs.core.ObjMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19157 = this;
  return this__19157.keys.length
};
cljs.core.ObjMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19158 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.ObjMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19159 = this;
  return new cljs.core.ObjMap(meta, this__19159.keys, this__19159.strobj, this__19159.update_count, this__19159.__hash)
};
cljs.core.ObjMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19160 = this;
  return this__19160.meta
};
cljs.core.ObjMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19161 = this;
  return cljs.core.with_meta.call(null, cljs.core.ObjMap.EMPTY, this__19161.meta)
};
cljs.core.ObjMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19162 = this;
  if(function() {
    var and__3822__auto____19163 = goog.isString(k);
    if(and__3822__auto____19163) {
      return!(cljs.core.scan_array.call(null, 1, k, this__19162.keys) == null)
    }else {
      return and__3822__auto____19163
    }
  }()) {
    var new_keys__19164 = this__19162.keys.slice();
    var new_strobj__19165 = cljs.core.obj_clone.call(null, this__19162.strobj, this__19162.keys);
    new_keys__19164.splice(cljs.core.scan_array.call(null, 1, k, new_keys__19164), 1);
    cljs.core.js_delete.call(null, new_strobj__19165, k);
    return new cljs.core.ObjMap(this__19162.meta, new_keys__19164, new_strobj__19165, this__19162.update_count + 1, null)
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
  var this__19170 = this;
  var h__2247__auto____19171 = this__19170.__hash;
  if(!(h__2247__auto____19171 == null)) {
    return h__2247__auto____19171
  }else {
    var h__2247__auto____19172 = cljs.core.hash_imap.call(null, coll);
    this__19170.__hash = h__2247__auto____19172;
    return h__2247__auto____19172
  }
};
cljs.core.HashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19173 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.HashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19174 = this;
  var bucket__19175 = this__19174.hashobj[cljs.core.hash.call(null, k)];
  var i__19176 = cljs.core.truth_(bucket__19175) ? cljs.core.scan_array.call(null, 2, k, bucket__19175) : null;
  if(cljs.core.truth_(i__19176)) {
    return bucket__19175[i__19176 + 1]
  }else {
    return not_found
  }
};
cljs.core.HashMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19177 = this;
  var h__19178 = cljs.core.hash.call(null, k);
  var bucket__19179 = this__19177.hashobj[h__19178];
  if(cljs.core.truth_(bucket__19179)) {
    var new_bucket__19180 = bucket__19179.slice();
    var new_hashobj__19181 = goog.object.clone(this__19177.hashobj);
    new_hashobj__19181[h__19178] = new_bucket__19180;
    var temp__3971__auto____19182 = cljs.core.scan_array.call(null, 2, k, new_bucket__19180);
    if(cljs.core.truth_(temp__3971__auto____19182)) {
      var i__19183 = temp__3971__auto____19182;
      new_bucket__19180[i__19183 + 1] = v;
      return new cljs.core.HashMap(this__19177.meta, this__19177.count, new_hashobj__19181, null)
    }else {
      new_bucket__19180.push(k, v);
      return new cljs.core.HashMap(this__19177.meta, this__19177.count + 1, new_hashobj__19181, null)
    }
  }else {
    var new_hashobj__19184 = goog.object.clone(this__19177.hashobj);
    new_hashobj__19184[h__19178] = [k, v];
    return new cljs.core.HashMap(this__19177.meta, this__19177.count + 1, new_hashobj__19184, null)
  }
};
cljs.core.HashMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19185 = this;
  var bucket__19186 = this__19185.hashobj[cljs.core.hash.call(null, k)];
  var i__19187 = cljs.core.truth_(bucket__19186) ? cljs.core.scan_array.call(null, 2, k, bucket__19186) : null;
  if(cljs.core.truth_(i__19187)) {
    return true
  }else {
    return false
  }
};
cljs.core.HashMap.prototype.call = function() {
  var G__19212 = null;
  var G__19212__2 = function(this_sym19188, k) {
    var this__19190 = this;
    var this_sym19188__19191 = this;
    var coll__19192 = this_sym19188__19191;
    return coll__19192.cljs$core$ILookup$_lookup$arity$2(coll__19192, k)
  };
  var G__19212__3 = function(this_sym19189, k, not_found) {
    var this__19190 = this;
    var this_sym19189__19193 = this;
    var coll__19194 = this_sym19189__19193;
    return coll__19194.cljs$core$ILookup$_lookup$arity$3(coll__19194, k, not_found)
  };
  G__19212 = function(this_sym19189, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19212__2.call(this, this_sym19189, k);
      case 3:
        return G__19212__3.call(this, this_sym19189, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19212
}();
cljs.core.HashMap.prototype.apply = function(this_sym19168, args19169) {
  var this__19195 = this;
  return this_sym19168.call.apply(this_sym19168, [this_sym19168].concat(args19169.slice()))
};
cljs.core.HashMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19196 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.HashMap.prototype.toString = function() {
  var this__19197 = this;
  var this__19198 = this;
  return cljs.core.pr_str.call(null, this__19198)
};
cljs.core.HashMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19199 = this;
  if(this__19199.count > 0) {
    var hashes__19200 = cljs.core.js_keys.call(null, this__19199.hashobj).sort();
    return cljs.core.mapcat.call(null, function(p1__19167_SHARP_) {
      return cljs.core.map.call(null, cljs.core.vec, cljs.core.partition.call(null, 2, this__19199.hashobj[p1__19167_SHARP_]))
    }, hashes__19200)
  }else {
    return null
  }
};
cljs.core.HashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19201 = this;
  return this__19201.count
};
cljs.core.HashMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19202 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.HashMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19203 = this;
  return new cljs.core.HashMap(meta, this__19203.count, this__19203.hashobj, this__19203.__hash)
};
cljs.core.HashMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19204 = this;
  return this__19204.meta
};
cljs.core.HashMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19205 = this;
  return cljs.core.with_meta.call(null, cljs.core.HashMap.EMPTY, this__19205.meta)
};
cljs.core.HashMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19206 = this;
  var h__19207 = cljs.core.hash.call(null, k);
  var bucket__19208 = this__19206.hashobj[h__19207];
  var i__19209 = cljs.core.truth_(bucket__19208) ? cljs.core.scan_array.call(null, 2, k, bucket__19208) : null;
  if(cljs.core.not.call(null, i__19209)) {
    return coll
  }else {
    var new_hashobj__19210 = goog.object.clone(this__19206.hashobj);
    if(3 > bucket__19208.length) {
      cljs.core.js_delete.call(null, new_hashobj__19210, h__19207)
    }else {
      var new_bucket__19211 = bucket__19208.slice();
      new_bucket__19211.splice(i__19209, 2);
      new_hashobj__19210[h__19207] = new_bucket__19211
    }
    return new cljs.core.HashMap(this__19206.meta, this__19206.count - 1, new_hashobj__19210, null)
  }
};
cljs.core.HashMap;
cljs.core.HashMap.EMPTY = new cljs.core.HashMap(null, 0, {}, 0);
cljs.core.HashMap.fromArrays = function(ks, vs) {
  var len__19213 = ks.length;
  var i__19214 = 0;
  var out__19215 = cljs.core.HashMap.EMPTY;
  while(true) {
    if(i__19214 < len__19213) {
      var G__19216 = i__19214 + 1;
      var G__19217 = cljs.core.assoc.call(null, out__19215, ks[i__19214], vs[i__19214]);
      i__19214 = G__19216;
      out__19215 = G__19217;
      continue
    }else {
      return out__19215
    }
    break
  }
};
cljs.core.array_map_index_of = function array_map_index_of(m, k) {
  var arr__19221 = m.arr;
  var len__19222 = arr__19221.length;
  var i__19223 = 0;
  while(true) {
    if(len__19222 <= i__19223) {
      return-1
    }else {
      if(cljs.core._EQ_.call(null, arr__19221[i__19223], k)) {
        return i__19223
      }else {
        if("\ufdd0'else") {
          var G__19224 = i__19223 + 2;
          i__19223 = G__19224;
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
  var this__19227 = this;
  return new cljs.core.TransientArrayMap({}, this__19227.arr.length, this__19227.arr.slice())
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19228 = this;
  var h__2247__auto____19229 = this__19228.__hash;
  if(!(h__2247__auto____19229 == null)) {
    return h__2247__auto____19229
  }else {
    var h__2247__auto____19230 = cljs.core.hash_imap.call(null, coll);
    this__19228.__hash = h__2247__auto____19230;
    return h__2247__auto____19230
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19231 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19232 = this;
  var idx__19233 = cljs.core.array_map_index_of.call(null, coll, k);
  if(idx__19233 === -1) {
    return not_found
  }else {
    return this__19232.arr[idx__19233 + 1]
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19234 = this;
  var idx__19235 = cljs.core.array_map_index_of.call(null, coll, k);
  if(idx__19235 === -1) {
    if(this__19234.cnt < cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD) {
      return new cljs.core.PersistentArrayMap(this__19234.meta, this__19234.cnt + 1, function() {
        var G__19236__19237 = this__19234.arr.slice();
        G__19236__19237.push(k);
        G__19236__19237.push(v);
        return G__19236__19237
      }(), null)
    }else {
      return cljs.core.persistent_BANG_.call(null, cljs.core.assoc_BANG_.call(null, cljs.core.transient$.call(null, cljs.core.into.call(null, cljs.core.PersistentHashMap.EMPTY, coll)), k, v))
    }
  }else {
    if(v === this__19234.arr[idx__19235 + 1]) {
      return coll
    }else {
      if("\ufdd0'else") {
        return new cljs.core.PersistentArrayMap(this__19234.meta, this__19234.cnt, function() {
          var G__19238__19239 = this__19234.arr.slice();
          G__19238__19239[idx__19235 + 1] = v;
          return G__19238__19239
        }(), null)
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19240 = this;
  return!(cljs.core.array_map_index_of.call(null, coll, k) === -1)
};
cljs.core.PersistentArrayMap.prototype.call = function() {
  var G__19272 = null;
  var G__19272__2 = function(this_sym19241, k) {
    var this__19243 = this;
    var this_sym19241__19244 = this;
    var coll__19245 = this_sym19241__19244;
    return coll__19245.cljs$core$ILookup$_lookup$arity$2(coll__19245, k)
  };
  var G__19272__3 = function(this_sym19242, k, not_found) {
    var this__19243 = this;
    var this_sym19242__19246 = this;
    var coll__19247 = this_sym19242__19246;
    return coll__19247.cljs$core$ILookup$_lookup$arity$3(coll__19247, k, not_found)
  };
  G__19272 = function(this_sym19242, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19272__2.call(this, this_sym19242, k);
      case 3:
        return G__19272__3.call(this, this_sym19242, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19272
}();
cljs.core.PersistentArrayMap.prototype.apply = function(this_sym19225, args19226) {
  var this__19248 = this;
  return this_sym19225.call.apply(this_sym19225, [this_sym19225].concat(args19226.slice()))
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(coll, f, init) {
  var this__19249 = this;
  var len__19250 = this__19249.arr.length;
  var i__19251 = 0;
  var init__19252 = init;
  while(true) {
    if(i__19251 < len__19250) {
      var init__19253 = f.call(null, init__19252, this__19249.arr[i__19251], this__19249.arr[i__19251 + 1]);
      if(cljs.core.reduced_QMARK_.call(null, init__19253)) {
        return cljs.core.deref.call(null, init__19253)
      }else {
        var G__19273 = i__19251 + 2;
        var G__19274 = init__19253;
        i__19251 = G__19273;
        init__19252 = G__19274;
        continue
      }
    }else {
      return null
    }
    break
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19254 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.PersistentArrayMap.prototype.toString = function() {
  var this__19255 = this;
  var this__19256 = this;
  return cljs.core.pr_str.call(null, this__19256)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19257 = this;
  if(this__19257.cnt > 0) {
    var len__19258 = this__19257.arr.length;
    var array_map_seq__19259 = function array_map_seq(i) {
      return new cljs.core.LazySeq(null, false, function() {
        if(i < len__19258) {
          return cljs.core.cons.call(null, cljs.core.PersistentVector.fromArray([this__19257.arr[i], this__19257.arr[i + 1]], true), array_map_seq.call(null, i + 2))
        }else {
          return null
        }
      }, null)
    };
    return array_map_seq__19259.call(null, 0)
  }else {
    return null
  }
};
cljs.core.PersistentArrayMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19260 = this;
  return this__19260.cnt
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19261 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19262 = this;
  return new cljs.core.PersistentArrayMap(meta, this__19262.cnt, this__19262.arr, this__19262.__hash)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19263 = this;
  return this__19263.meta
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19264 = this;
  return cljs.core._with_meta.call(null, cljs.core.PersistentArrayMap.EMPTY, this__19264.meta)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19265 = this;
  var idx__19266 = cljs.core.array_map_index_of.call(null, coll, k);
  if(idx__19266 >= 0) {
    var len__19267 = this__19265.arr.length;
    var new_len__19268 = len__19267 - 2;
    if(new_len__19268 === 0) {
      return coll.cljs$core$IEmptyableCollection$_empty$arity$1(coll)
    }else {
      var new_arr__19269 = cljs.core.make_array.call(null, new_len__19268);
      var s__19270 = 0;
      var d__19271 = 0;
      while(true) {
        if(s__19270 >= len__19267) {
          return new cljs.core.PersistentArrayMap(this__19265.meta, this__19265.cnt - 1, new_arr__19269, null)
        }else {
          if(cljs.core._EQ_.call(null, k, this__19265.arr[s__19270])) {
            var G__19275 = s__19270 + 2;
            var G__19276 = d__19271;
            s__19270 = G__19275;
            d__19271 = G__19276;
            continue
          }else {
            if("\ufdd0'else") {
              new_arr__19269[d__19271] = this__19265.arr[s__19270];
              new_arr__19269[d__19271 + 1] = this__19265.arr[s__19270 + 1];
              var G__19277 = s__19270 + 2;
              var G__19278 = d__19271 + 2;
              s__19270 = G__19277;
              d__19271 = G__19278;
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
  var len__19279 = cljs.core.count.call(null, ks);
  var i__19280 = 0;
  var out__19281 = cljs.core.transient$.call(null, cljs.core.PersistentArrayMap.EMPTY);
  while(true) {
    if(i__19280 < len__19279) {
      var G__19282 = i__19280 + 1;
      var G__19283 = cljs.core.assoc_BANG_.call(null, out__19281, ks[i__19280], vs[i__19280]);
      i__19280 = G__19282;
      out__19281 = G__19283;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, out__19281)
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
  var this__19284 = this;
  if(cljs.core.truth_(this__19284.editable_QMARK_)) {
    var idx__19285 = cljs.core.array_map_index_of.call(null, tcoll, key);
    if(idx__19285 >= 0) {
      this__19284.arr[idx__19285] = this__19284.arr[this__19284.len - 2];
      this__19284.arr[idx__19285 + 1] = this__19284.arr[this__19284.len - 1];
      var G__19286__19287 = this__19284.arr;
      G__19286__19287.pop();
      G__19286__19287.pop();
      G__19286__19287;
      this__19284.len = this__19284.len - 2
    }else {
    }
    return tcoll
  }else {
    throw new Error("dissoc! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(tcoll, key, val) {
  var this__19288 = this;
  if(cljs.core.truth_(this__19288.editable_QMARK_)) {
    var idx__19289 = cljs.core.array_map_index_of.call(null, tcoll, key);
    if(idx__19289 === -1) {
      if(this__19288.len + 2 <= 2 * cljs.core.PersistentArrayMap.HASHMAP_THRESHOLD) {
        this__19288.len = this__19288.len + 2;
        this__19288.arr.push(key);
        this__19288.arr.push(val);
        return tcoll
      }else {
        return cljs.core.assoc_BANG_.call(null, cljs.core.array__GT_transient_hash_map.call(null, this__19288.len, this__19288.arr), key, val)
      }
    }else {
      if(val === this__19288.arr[idx__19289 + 1]) {
        return tcoll
      }else {
        this__19288.arr[idx__19289 + 1] = val;
        return tcoll
      }
    }
  }else {
    throw new Error("assoc! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, o) {
  var this__19290 = this;
  if(cljs.core.truth_(this__19290.editable_QMARK_)) {
    if(function() {
      var G__19291__19292 = o;
      if(G__19291__19292) {
        if(function() {
          var or__3824__auto____19293 = G__19291__19292.cljs$lang$protocol_mask$partition0$ & 2048;
          if(or__3824__auto____19293) {
            return or__3824__auto____19293
          }else {
            return G__19291__19292.cljs$core$IMapEntry$
          }
        }()) {
          return true
        }else {
          if(!G__19291__19292.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__19291__19292)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__19291__19292)
      }
    }()) {
      return tcoll.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(tcoll, cljs.core.key.call(null, o), cljs.core.val.call(null, o))
    }else {
      var es__19294 = cljs.core.seq.call(null, o);
      var tcoll__19295 = tcoll;
      while(true) {
        var temp__3971__auto____19296 = cljs.core.first.call(null, es__19294);
        if(cljs.core.truth_(temp__3971__auto____19296)) {
          var e__19297 = temp__3971__auto____19296;
          var G__19303 = cljs.core.next.call(null, es__19294);
          var G__19304 = tcoll__19295.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3(tcoll__19295, cljs.core.key.call(null, e__19297), cljs.core.val.call(null, e__19297));
          es__19294 = G__19303;
          tcoll__19295 = G__19304;
          continue
        }else {
          return tcoll__19295
        }
        break
      }
    }
  }else {
    throw new Error("conj! after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__19298 = this;
  if(cljs.core.truth_(this__19298.editable_QMARK_)) {
    this__19298.editable_QMARK_ = false;
    return new cljs.core.PersistentArrayMap(null, cljs.core.quot.call(null, this__19298.len, 2), this__19298.arr, null)
  }else {
    throw new Error("persistent! called twice");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(tcoll, k) {
  var this__19299 = this;
  return tcoll.cljs$core$ILookup$_lookup$arity$3(tcoll, k, null)
};
cljs.core.TransientArrayMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(tcoll, k, not_found) {
  var this__19300 = this;
  if(cljs.core.truth_(this__19300.editable_QMARK_)) {
    var idx__19301 = cljs.core.array_map_index_of.call(null, tcoll, k);
    if(idx__19301 === -1) {
      return not_found
    }else {
      return this__19300.arr[idx__19301 + 1]
    }
  }else {
    throw new Error("lookup after persistent!");
  }
};
cljs.core.TransientArrayMap.prototype.cljs$core$ICounted$_count$arity$1 = function(tcoll) {
  var this__19302 = this;
  if(cljs.core.truth_(this__19302.editable_QMARK_)) {
    return cljs.core.quot.call(null, this__19302.len, 2)
  }else {
    throw new Error("count after persistent!");
  }
};
cljs.core.TransientArrayMap;
cljs.core.array__GT_transient_hash_map = function array__GT_transient_hash_map(len, arr) {
  var out__19307 = cljs.core.transient$.call(null, cljs.core.ObjMap.EMPTY);
  var i__19308 = 0;
  while(true) {
    if(i__19308 < len) {
      var G__19309 = cljs.core.assoc_BANG_.call(null, out__19307, arr[i__19308], arr[i__19308 + 1]);
      var G__19310 = i__19308 + 2;
      out__19307 = G__19309;
      i__19308 = G__19310;
      continue
    }else {
      return out__19307
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
    var G__19315__19316 = arr.slice();
    G__19315__19316[i] = a;
    return G__19315__19316
  };
  var clone_and_set__5 = function(arr, i, a, j, b) {
    var G__19317__19318 = arr.slice();
    G__19317__19318[i] = a;
    G__19317__19318[j] = b;
    return G__19317__19318
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
  var new_arr__19320 = cljs.core.make_array.call(null, arr.length - 2);
  cljs.core.array_copy.call(null, arr, 0, new_arr__19320, 0, 2 * i);
  cljs.core.array_copy.call(null, arr, 2 * (i + 1), new_arr__19320, 2 * i, new_arr__19320.length - 2 * i);
  return new_arr__19320
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
    var editable__19323 = inode.ensure_editable(edit);
    editable__19323.arr[i] = a;
    return editable__19323
  };
  var edit_and_set__6 = function(inode, edit, i, a, j, b) {
    var editable__19324 = inode.ensure_editable(edit);
    editable__19324.arr[i] = a;
    editable__19324.arr[j] = b;
    return editable__19324
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
  var len__19331 = arr.length;
  var i__19332 = 0;
  var init__19333 = init;
  while(true) {
    if(i__19332 < len__19331) {
      var init__19336 = function() {
        var k__19334 = arr[i__19332];
        if(!(k__19334 == null)) {
          return f.call(null, init__19333, k__19334, arr[i__19332 + 1])
        }else {
          var node__19335 = arr[i__19332 + 1];
          if(!(node__19335 == null)) {
            return node__19335.kv_reduce(f, init__19333)
          }else {
            return init__19333
          }
        }
      }();
      if(cljs.core.reduced_QMARK_.call(null, init__19336)) {
        return cljs.core.deref.call(null, init__19336)
      }else {
        var G__19337 = i__19332 + 2;
        var G__19338 = init__19336;
        i__19332 = G__19337;
        init__19333 = G__19338;
        continue
      }
    }else {
      return init__19333
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
  var this__19339 = this;
  var inode__19340 = this;
  if(this__19339.bitmap === bit) {
    return null
  }else {
    var editable__19341 = inode__19340.ensure_editable(e);
    var earr__19342 = editable__19341.arr;
    var len__19343 = earr__19342.length;
    editable__19341.bitmap = bit ^ editable__19341.bitmap;
    cljs.core.array_copy.call(null, earr__19342, 2 * (i + 1), earr__19342, 2 * i, len__19343 - 2 * (i + 1));
    earr__19342[len__19343 - 2] = null;
    earr__19342[len__19343 - 1] = null;
    return editable__19341
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_assoc_BANG_ = function(edit, shift, hash, key, val, added_leaf_QMARK_) {
  var this__19344 = this;
  var inode__19345 = this;
  var bit__19346 = 1 << (hash >>> shift & 31);
  var idx__19347 = cljs.core.bitmap_indexed_node_index.call(null, this__19344.bitmap, bit__19346);
  if((this__19344.bitmap & bit__19346) === 0) {
    var n__19348 = cljs.core.bit_count.call(null, this__19344.bitmap);
    if(2 * n__19348 < this__19344.arr.length) {
      var editable__19349 = inode__19345.ensure_editable(edit);
      var earr__19350 = editable__19349.arr;
      added_leaf_QMARK_.val = true;
      cljs.core.array_copy_downward.call(null, earr__19350, 2 * idx__19347, earr__19350, 2 * (idx__19347 + 1), 2 * (n__19348 - idx__19347));
      earr__19350[2 * idx__19347] = key;
      earr__19350[2 * idx__19347 + 1] = val;
      editable__19349.bitmap = editable__19349.bitmap | bit__19346;
      return editable__19349
    }else {
      if(n__19348 >= 16) {
        var nodes__19351 = cljs.core.make_array.call(null, 32);
        var jdx__19352 = hash >>> shift & 31;
        nodes__19351[jdx__19352] = cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_);
        var i__19353 = 0;
        var j__19354 = 0;
        while(true) {
          if(i__19353 < 32) {
            if((this__19344.bitmap >>> i__19353 & 1) === 0) {
              var G__19407 = i__19353 + 1;
              var G__19408 = j__19354;
              i__19353 = G__19407;
              j__19354 = G__19408;
              continue
            }else {
              nodes__19351[i__19353] = !(this__19344.arr[j__19354] == null) ? cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift + 5, cljs.core.hash.call(null, this__19344.arr[j__19354]), this__19344.arr[j__19354], this__19344.arr[j__19354 + 1], added_leaf_QMARK_) : this__19344.arr[j__19354 + 1];
              var G__19409 = i__19353 + 1;
              var G__19410 = j__19354 + 2;
              i__19353 = G__19409;
              j__19354 = G__19410;
              continue
            }
          }else {
          }
          break
        }
        return new cljs.core.ArrayNode(edit, n__19348 + 1, nodes__19351)
      }else {
        if("\ufdd0'else") {
          var new_arr__19355 = cljs.core.make_array.call(null, 2 * (n__19348 + 4));
          cljs.core.array_copy.call(null, this__19344.arr, 0, new_arr__19355, 0, 2 * idx__19347);
          new_arr__19355[2 * idx__19347] = key;
          new_arr__19355[2 * idx__19347 + 1] = val;
          cljs.core.array_copy.call(null, this__19344.arr, 2 * idx__19347, new_arr__19355, 2 * (idx__19347 + 1), 2 * (n__19348 - idx__19347));
          added_leaf_QMARK_.val = true;
          var editable__19356 = inode__19345.ensure_editable(edit);
          editable__19356.arr = new_arr__19355;
          editable__19356.bitmap = editable__19356.bitmap | bit__19346;
          return editable__19356
        }else {
          return null
        }
      }
    }
  }else {
    var key_or_nil__19357 = this__19344.arr[2 * idx__19347];
    var val_or_node__19358 = this__19344.arr[2 * idx__19347 + 1];
    if(key_or_nil__19357 == null) {
      var n__19359 = val_or_node__19358.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_);
      if(n__19359 === val_or_node__19358) {
        return inode__19345
      }else {
        return cljs.core.edit_and_set.call(null, inode__19345, edit, 2 * idx__19347 + 1, n__19359)
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19357)) {
        if(val === val_or_node__19358) {
          return inode__19345
        }else {
          return cljs.core.edit_and_set.call(null, inode__19345, edit, 2 * idx__19347 + 1, val)
        }
      }else {
        if("\ufdd0'else") {
          added_leaf_QMARK_.val = true;
          return cljs.core.edit_and_set.call(null, inode__19345, edit, 2 * idx__19347, null, 2 * idx__19347 + 1, cljs.core.create_node.call(null, edit, shift + 5, key_or_nil__19357, val_or_node__19358, hash, key, val))
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_seq = function() {
  var this__19360 = this;
  var inode__19361 = this;
  return cljs.core.create_inode_seq.call(null, this__19360.arr)
};
cljs.core.BitmapIndexedNode.prototype.inode_without_BANG_ = function(edit, shift, hash, key, removed_leaf_QMARK_) {
  var this__19362 = this;
  var inode__19363 = this;
  var bit__19364 = 1 << (hash >>> shift & 31);
  if((this__19362.bitmap & bit__19364) === 0) {
    return inode__19363
  }else {
    var idx__19365 = cljs.core.bitmap_indexed_node_index.call(null, this__19362.bitmap, bit__19364);
    var key_or_nil__19366 = this__19362.arr[2 * idx__19365];
    var val_or_node__19367 = this__19362.arr[2 * idx__19365 + 1];
    if(key_or_nil__19366 == null) {
      var n__19368 = val_or_node__19367.inode_without_BANG_(edit, shift + 5, hash, key, removed_leaf_QMARK_);
      if(n__19368 === val_or_node__19367) {
        return inode__19363
      }else {
        if(!(n__19368 == null)) {
          return cljs.core.edit_and_set.call(null, inode__19363, edit, 2 * idx__19365 + 1, n__19368)
        }else {
          if(this__19362.bitmap === bit__19364) {
            return null
          }else {
            if("\ufdd0'else") {
              return inode__19363.edit_and_remove_pair(edit, bit__19364, idx__19365)
            }else {
              return null
            }
          }
        }
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19366)) {
        removed_leaf_QMARK_[0] = true;
        return inode__19363.edit_and_remove_pair(edit, bit__19364, idx__19365)
      }else {
        if("\ufdd0'else") {
          return inode__19363
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.ensure_editable = function(e) {
  var this__19369 = this;
  var inode__19370 = this;
  if(e === this__19369.edit) {
    return inode__19370
  }else {
    var n__19371 = cljs.core.bit_count.call(null, this__19369.bitmap);
    var new_arr__19372 = cljs.core.make_array.call(null, n__19371 < 0 ? 4 : 2 * (n__19371 + 1));
    cljs.core.array_copy.call(null, this__19369.arr, 0, new_arr__19372, 0, 2 * n__19371);
    return new cljs.core.BitmapIndexedNode(e, this__19369.bitmap, new_arr__19372)
  }
};
cljs.core.BitmapIndexedNode.prototype.kv_reduce = function(f, init) {
  var this__19373 = this;
  var inode__19374 = this;
  return cljs.core.inode_kv_reduce.call(null, this__19373.arr, f, init)
};
cljs.core.BitmapIndexedNode.prototype.inode_find = function(shift, hash, key, not_found) {
  var this__19375 = this;
  var inode__19376 = this;
  var bit__19377 = 1 << (hash >>> shift & 31);
  if((this__19375.bitmap & bit__19377) === 0) {
    return not_found
  }else {
    var idx__19378 = cljs.core.bitmap_indexed_node_index.call(null, this__19375.bitmap, bit__19377);
    var key_or_nil__19379 = this__19375.arr[2 * idx__19378];
    var val_or_node__19380 = this__19375.arr[2 * idx__19378 + 1];
    if(key_or_nil__19379 == null) {
      return val_or_node__19380.inode_find(shift + 5, hash, key, not_found)
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19379)) {
        return cljs.core.PersistentVector.fromArray([key_or_nil__19379, val_or_node__19380], true)
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
  var this__19381 = this;
  var inode__19382 = this;
  var bit__19383 = 1 << (hash >>> shift & 31);
  if((this__19381.bitmap & bit__19383) === 0) {
    return inode__19382
  }else {
    var idx__19384 = cljs.core.bitmap_indexed_node_index.call(null, this__19381.bitmap, bit__19383);
    var key_or_nil__19385 = this__19381.arr[2 * idx__19384];
    var val_or_node__19386 = this__19381.arr[2 * idx__19384 + 1];
    if(key_or_nil__19385 == null) {
      var n__19387 = val_or_node__19386.inode_without(shift + 5, hash, key);
      if(n__19387 === val_or_node__19386) {
        return inode__19382
      }else {
        if(!(n__19387 == null)) {
          return new cljs.core.BitmapIndexedNode(null, this__19381.bitmap, cljs.core.clone_and_set.call(null, this__19381.arr, 2 * idx__19384 + 1, n__19387))
        }else {
          if(this__19381.bitmap === bit__19383) {
            return null
          }else {
            if("\ufdd0'else") {
              return new cljs.core.BitmapIndexedNode(null, this__19381.bitmap ^ bit__19383, cljs.core.remove_pair.call(null, this__19381.arr, idx__19384))
            }else {
              return null
            }
          }
        }
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19385)) {
        return new cljs.core.BitmapIndexedNode(null, this__19381.bitmap ^ bit__19383, cljs.core.remove_pair.call(null, this__19381.arr, idx__19384))
      }else {
        if("\ufdd0'else") {
          return inode__19382
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_assoc = function(shift, hash, key, val, added_leaf_QMARK_) {
  var this__19388 = this;
  var inode__19389 = this;
  var bit__19390 = 1 << (hash >>> shift & 31);
  var idx__19391 = cljs.core.bitmap_indexed_node_index.call(null, this__19388.bitmap, bit__19390);
  if((this__19388.bitmap & bit__19390) === 0) {
    var n__19392 = cljs.core.bit_count.call(null, this__19388.bitmap);
    if(n__19392 >= 16) {
      var nodes__19393 = cljs.core.make_array.call(null, 32);
      var jdx__19394 = hash >>> shift & 31;
      nodes__19393[jdx__19394] = cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_);
      var i__19395 = 0;
      var j__19396 = 0;
      while(true) {
        if(i__19395 < 32) {
          if((this__19388.bitmap >>> i__19395 & 1) === 0) {
            var G__19411 = i__19395 + 1;
            var G__19412 = j__19396;
            i__19395 = G__19411;
            j__19396 = G__19412;
            continue
          }else {
            nodes__19393[i__19395] = !(this__19388.arr[j__19396] == null) ? cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift + 5, cljs.core.hash.call(null, this__19388.arr[j__19396]), this__19388.arr[j__19396], this__19388.arr[j__19396 + 1], added_leaf_QMARK_) : this__19388.arr[j__19396 + 1];
            var G__19413 = i__19395 + 1;
            var G__19414 = j__19396 + 2;
            i__19395 = G__19413;
            j__19396 = G__19414;
            continue
          }
        }else {
        }
        break
      }
      return new cljs.core.ArrayNode(null, n__19392 + 1, nodes__19393)
    }else {
      var new_arr__19397 = cljs.core.make_array.call(null, 2 * (n__19392 + 1));
      cljs.core.array_copy.call(null, this__19388.arr, 0, new_arr__19397, 0, 2 * idx__19391);
      new_arr__19397[2 * idx__19391] = key;
      new_arr__19397[2 * idx__19391 + 1] = val;
      cljs.core.array_copy.call(null, this__19388.arr, 2 * idx__19391, new_arr__19397, 2 * (idx__19391 + 1), 2 * (n__19392 - idx__19391));
      added_leaf_QMARK_.val = true;
      return new cljs.core.BitmapIndexedNode(null, this__19388.bitmap | bit__19390, new_arr__19397)
    }
  }else {
    var key_or_nil__19398 = this__19388.arr[2 * idx__19391];
    var val_or_node__19399 = this__19388.arr[2 * idx__19391 + 1];
    if(key_or_nil__19398 == null) {
      var n__19400 = val_or_node__19399.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_);
      if(n__19400 === val_or_node__19399) {
        return inode__19389
      }else {
        return new cljs.core.BitmapIndexedNode(null, this__19388.bitmap, cljs.core.clone_and_set.call(null, this__19388.arr, 2 * idx__19391 + 1, n__19400))
      }
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19398)) {
        if(val === val_or_node__19399) {
          return inode__19389
        }else {
          return new cljs.core.BitmapIndexedNode(null, this__19388.bitmap, cljs.core.clone_and_set.call(null, this__19388.arr, 2 * idx__19391 + 1, val))
        }
      }else {
        if("\ufdd0'else") {
          added_leaf_QMARK_.val = true;
          return new cljs.core.BitmapIndexedNode(null, this__19388.bitmap, cljs.core.clone_and_set.call(null, this__19388.arr, 2 * idx__19391, null, 2 * idx__19391 + 1, cljs.core.create_node.call(null, shift + 5, key_or_nil__19398, val_or_node__19399, hash, key, val)))
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.BitmapIndexedNode.prototype.inode_lookup = function(shift, hash, key, not_found) {
  var this__19401 = this;
  var inode__19402 = this;
  var bit__19403 = 1 << (hash >>> shift & 31);
  if((this__19401.bitmap & bit__19403) === 0) {
    return not_found
  }else {
    var idx__19404 = cljs.core.bitmap_indexed_node_index.call(null, this__19401.bitmap, bit__19403);
    var key_or_nil__19405 = this__19401.arr[2 * idx__19404];
    var val_or_node__19406 = this__19401.arr[2 * idx__19404 + 1];
    if(key_or_nil__19405 == null) {
      return val_or_node__19406.inode_lookup(shift + 5, hash, key, not_found)
    }else {
      if(cljs.core.key_test.call(null, key, key_or_nil__19405)) {
        return val_or_node__19406
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
  var arr__19422 = array_node.arr;
  var len__19423 = 2 * (array_node.cnt - 1);
  var new_arr__19424 = cljs.core.make_array.call(null, len__19423);
  var i__19425 = 0;
  var j__19426 = 1;
  var bitmap__19427 = 0;
  while(true) {
    if(i__19425 < len__19423) {
      if(function() {
        var and__3822__auto____19428 = !(i__19425 === idx);
        if(and__3822__auto____19428) {
          return!(arr__19422[i__19425] == null)
        }else {
          return and__3822__auto____19428
        }
      }()) {
        new_arr__19424[j__19426] = arr__19422[i__19425];
        var G__19429 = i__19425 + 1;
        var G__19430 = j__19426 + 2;
        var G__19431 = bitmap__19427 | 1 << i__19425;
        i__19425 = G__19429;
        j__19426 = G__19430;
        bitmap__19427 = G__19431;
        continue
      }else {
        var G__19432 = i__19425 + 1;
        var G__19433 = j__19426;
        var G__19434 = bitmap__19427;
        i__19425 = G__19432;
        j__19426 = G__19433;
        bitmap__19427 = G__19434;
        continue
      }
    }else {
      return new cljs.core.BitmapIndexedNode(edit, bitmap__19427, new_arr__19424)
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
  var this__19435 = this;
  var inode__19436 = this;
  var idx__19437 = hash >>> shift & 31;
  var node__19438 = this__19435.arr[idx__19437];
  if(node__19438 == null) {
    var editable__19439 = cljs.core.edit_and_set.call(null, inode__19436, edit, idx__19437, cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_));
    editable__19439.cnt = editable__19439.cnt + 1;
    return editable__19439
  }else {
    var n__19440 = node__19438.inode_assoc_BANG_(edit, shift + 5, hash, key, val, added_leaf_QMARK_);
    if(n__19440 === node__19438) {
      return inode__19436
    }else {
      return cljs.core.edit_and_set.call(null, inode__19436, edit, idx__19437, n__19440)
    }
  }
};
cljs.core.ArrayNode.prototype.inode_seq = function() {
  var this__19441 = this;
  var inode__19442 = this;
  return cljs.core.create_array_node_seq.call(null, this__19441.arr)
};
cljs.core.ArrayNode.prototype.inode_without_BANG_ = function(edit, shift, hash, key, removed_leaf_QMARK_) {
  var this__19443 = this;
  var inode__19444 = this;
  var idx__19445 = hash >>> shift & 31;
  var node__19446 = this__19443.arr[idx__19445];
  if(node__19446 == null) {
    return inode__19444
  }else {
    var n__19447 = node__19446.inode_without_BANG_(edit, shift + 5, hash, key, removed_leaf_QMARK_);
    if(n__19447 === node__19446) {
      return inode__19444
    }else {
      if(n__19447 == null) {
        if(this__19443.cnt <= 8) {
          return cljs.core.pack_array_node.call(null, inode__19444, edit, idx__19445)
        }else {
          var editable__19448 = cljs.core.edit_and_set.call(null, inode__19444, edit, idx__19445, n__19447);
          editable__19448.cnt = editable__19448.cnt - 1;
          return editable__19448
        }
      }else {
        if("\ufdd0'else") {
          return cljs.core.edit_and_set.call(null, inode__19444, edit, idx__19445, n__19447)
        }else {
          return null
        }
      }
    }
  }
};
cljs.core.ArrayNode.prototype.ensure_editable = function(e) {
  var this__19449 = this;
  var inode__19450 = this;
  if(e === this__19449.edit) {
    return inode__19450
  }else {
    return new cljs.core.ArrayNode(e, this__19449.cnt, this__19449.arr.slice())
  }
};
cljs.core.ArrayNode.prototype.kv_reduce = function(f, init) {
  var this__19451 = this;
  var inode__19452 = this;
  var len__19453 = this__19451.arr.length;
  var i__19454 = 0;
  var init__19455 = init;
  while(true) {
    if(i__19454 < len__19453) {
      var node__19456 = this__19451.arr[i__19454];
      if(!(node__19456 == null)) {
        var init__19457 = node__19456.kv_reduce(f, init__19455);
        if(cljs.core.reduced_QMARK_.call(null, init__19457)) {
          return cljs.core.deref.call(null, init__19457)
        }else {
          var G__19476 = i__19454 + 1;
          var G__19477 = init__19457;
          i__19454 = G__19476;
          init__19455 = G__19477;
          continue
        }
      }else {
        return null
      }
    }else {
      return init__19455
    }
    break
  }
};
cljs.core.ArrayNode.prototype.inode_find = function(shift, hash, key, not_found) {
  var this__19458 = this;
  var inode__19459 = this;
  var idx__19460 = hash >>> shift & 31;
  var node__19461 = this__19458.arr[idx__19460];
  if(!(node__19461 == null)) {
    return node__19461.inode_find(shift + 5, hash, key, not_found)
  }else {
    return not_found
  }
};
cljs.core.ArrayNode.prototype.inode_without = function(shift, hash, key) {
  var this__19462 = this;
  var inode__19463 = this;
  var idx__19464 = hash >>> shift & 31;
  var node__19465 = this__19462.arr[idx__19464];
  if(!(node__19465 == null)) {
    var n__19466 = node__19465.inode_without(shift + 5, hash, key);
    if(n__19466 === node__19465) {
      return inode__19463
    }else {
      if(n__19466 == null) {
        if(this__19462.cnt <= 8) {
          return cljs.core.pack_array_node.call(null, inode__19463, null, idx__19464)
        }else {
          return new cljs.core.ArrayNode(null, this__19462.cnt - 1, cljs.core.clone_and_set.call(null, this__19462.arr, idx__19464, n__19466))
        }
      }else {
        if("\ufdd0'else") {
          return new cljs.core.ArrayNode(null, this__19462.cnt, cljs.core.clone_and_set.call(null, this__19462.arr, idx__19464, n__19466))
        }else {
          return null
        }
      }
    }
  }else {
    return inode__19463
  }
};
cljs.core.ArrayNode.prototype.inode_assoc = function(shift, hash, key, val, added_leaf_QMARK_) {
  var this__19467 = this;
  var inode__19468 = this;
  var idx__19469 = hash >>> shift & 31;
  var node__19470 = this__19467.arr[idx__19469];
  if(node__19470 == null) {
    return new cljs.core.ArrayNode(null, this__19467.cnt + 1, cljs.core.clone_and_set.call(null, this__19467.arr, idx__19469, cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_)))
  }else {
    var n__19471 = node__19470.inode_assoc(shift + 5, hash, key, val, added_leaf_QMARK_);
    if(n__19471 === node__19470) {
      return inode__19468
    }else {
      return new cljs.core.ArrayNode(null, this__19467.cnt, cljs.core.clone_and_set.call(null, this__19467.arr, idx__19469, n__19471))
    }
  }
};
cljs.core.ArrayNode.prototype.inode_lookup = function(shift, hash, key, not_found) {
  var this__19472 = this;
  var inode__19473 = this;
  var idx__19474 = hash >>> shift & 31;
  var node__19475 = this__19472.arr[idx__19474];
  if(!(node__19475 == null)) {
    return node__19475.inode_lookup(shift + 5, hash, key, not_found)
  }else {
    return not_found
  }
};
cljs.core.ArrayNode;
cljs.core.hash_collision_node_find_index = function hash_collision_node_find_index(arr, cnt, key) {
  var lim__19480 = 2 * cnt;
  var i__19481 = 0;
  while(true) {
    if(i__19481 < lim__19480) {
      if(cljs.core.key_test.call(null, key, arr[i__19481])) {
        return i__19481
      }else {
        var G__19482 = i__19481 + 2;
        i__19481 = G__19482;
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
  var this__19483 = this;
  var inode__19484 = this;
  if(hash === this__19483.collision_hash) {
    var idx__19485 = cljs.core.hash_collision_node_find_index.call(null, this__19483.arr, this__19483.cnt, key);
    if(idx__19485 === -1) {
      if(this__19483.arr.length > 2 * this__19483.cnt) {
        var editable__19486 = cljs.core.edit_and_set.call(null, inode__19484, edit, 2 * this__19483.cnt, key, 2 * this__19483.cnt + 1, val);
        added_leaf_QMARK_.val = true;
        editable__19486.cnt = editable__19486.cnt + 1;
        return editable__19486
      }else {
        var len__19487 = this__19483.arr.length;
        var new_arr__19488 = cljs.core.make_array.call(null, len__19487 + 2);
        cljs.core.array_copy.call(null, this__19483.arr, 0, new_arr__19488, 0, len__19487);
        new_arr__19488[len__19487] = key;
        new_arr__19488[len__19487 + 1] = val;
        added_leaf_QMARK_.val = true;
        return inode__19484.ensure_editable_array(edit, this__19483.cnt + 1, new_arr__19488)
      }
    }else {
      if(this__19483.arr[idx__19485 + 1] === val) {
        return inode__19484
      }else {
        return cljs.core.edit_and_set.call(null, inode__19484, edit, idx__19485 + 1, val)
      }
    }
  }else {
    return(new cljs.core.BitmapIndexedNode(edit, 1 << (this__19483.collision_hash >>> shift & 31), [null, inode__19484, null, null])).inode_assoc_BANG_(edit, shift, hash, key, val, added_leaf_QMARK_)
  }
};
cljs.core.HashCollisionNode.prototype.inode_seq = function() {
  var this__19489 = this;
  var inode__19490 = this;
  return cljs.core.create_inode_seq.call(null, this__19489.arr)
};
cljs.core.HashCollisionNode.prototype.inode_without_BANG_ = function(edit, shift, hash, key, removed_leaf_QMARK_) {
  var this__19491 = this;
  var inode__19492 = this;
  var idx__19493 = cljs.core.hash_collision_node_find_index.call(null, this__19491.arr, this__19491.cnt, key);
  if(idx__19493 === -1) {
    return inode__19492
  }else {
    removed_leaf_QMARK_[0] = true;
    if(this__19491.cnt === 1) {
      return null
    }else {
      var editable__19494 = inode__19492.ensure_editable(edit);
      var earr__19495 = editable__19494.arr;
      earr__19495[idx__19493] = earr__19495[2 * this__19491.cnt - 2];
      earr__19495[idx__19493 + 1] = earr__19495[2 * this__19491.cnt - 1];
      earr__19495[2 * this__19491.cnt - 1] = null;
      earr__19495[2 * this__19491.cnt - 2] = null;
      editable__19494.cnt = editable__19494.cnt - 1;
      return editable__19494
    }
  }
};
cljs.core.HashCollisionNode.prototype.ensure_editable = function(e) {
  var this__19496 = this;
  var inode__19497 = this;
  if(e === this__19496.edit) {
    return inode__19497
  }else {
    var new_arr__19498 = cljs.core.make_array.call(null, 2 * (this__19496.cnt + 1));
    cljs.core.array_copy.call(null, this__19496.arr, 0, new_arr__19498, 0, 2 * this__19496.cnt);
    return new cljs.core.HashCollisionNode(e, this__19496.collision_hash, this__19496.cnt, new_arr__19498)
  }
};
cljs.core.HashCollisionNode.prototype.kv_reduce = function(f, init) {
  var this__19499 = this;
  var inode__19500 = this;
  return cljs.core.inode_kv_reduce.call(null, this__19499.arr, f, init)
};
cljs.core.HashCollisionNode.prototype.inode_find = function(shift, hash, key, not_found) {
  var this__19501 = this;
  var inode__19502 = this;
  var idx__19503 = cljs.core.hash_collision_node_find_index.call(null, this__19501.arr, this__19501.cnt, key);
  if(idx__19503 < 0) {
    return not_found
  }else {
    if(cljs.core.key_test.call(null, key, this__19501.arr[idx__19503])) {
      return cljs.core.PersistentVector.fromArray([this__19501.arr[idx__19503], this__19501.arr[idx__19503 + 1]], true)
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
  var this__19504 = this;
  var inode__19505 = this;
  var idx__19506 = cljs.core.hash_collision_node_find_index.call(null, this__19504.arr, this__19504.cnt, key);
  if(idx__19506 === -1) {
    return inode__19505
  }else {
    if(this__19504.cnt === 1) {
      return null
    }else {
      if("\ufdd0'else") {
        return new cljs.core.HashCollisionNode(null, this__19504.collision_hash, this__19504.cnt - 1, cljs.core.remove_pair.call(null, this__19504.arr, cljs.core.quot.call(null, idx__19506, 2)))
      }else {
        return null
      }
    }
  }
};
cljs.core.HashCollisionNode.prototype.inode_assoc = function(shift, hash, key, val, added_leaf_QMARK_) {
  var this__19507 = this;
  var inode__19508 = this;
  if(hash === this__19507.collision_hash) {
    var idx__19509 = cljs.core.hash_collision_node_find_index.call(null, this__19507.arr, this__19507.cnt, key);
    if(idx__19509 === -1) {
      var len__19510 = this__19507.arr.length;
      var new_arr__19511 = cljs.core.make_array.call(null, len__19510 + 2);
      cljs.core.array_copy.call(null, this__19507.arr, 0, new_arr__19511, 0, len__19510);
      new_arr__19511[len__19510] = key;
      new_arr__19511[len__19510 + 1] = val;
      added_leaf_QMARK_.val = true;
      return new cljs.core.HashCollisionNode(null, this__19507.collision_hash, this__19507.cnt + 1, new_arr__19511)
    }else {
      if(cljs.core._EQ_.call(null, this__19507.arr[idx__19509], val)) {
        return inode__19508
      }else {
        return new cljs.core.HashCollisionNode(null, this__19507.collision_hash, this__19507.cnt, cljs.core.clone_and_set.call(null, this__19507.arr, idx__19509 + 1, val))
      }
    }
  }else {
    return(new cljs.core.BitmapIndexedNode(null, 1 << (this__19507.collision_hash >>> shift & 31), [null, inode__19508])).inode_assoc(shift, hash, key, val, added_leaf_QMARK_)
  }
};
cljs.core.HashCollisionNode.prototype.inode_lookup = function(shift, hash, key, not_found) {
  var this__19512 = this;
  var inode__19513 = this;
  var idx__19514 = cljs.core.hash_collision_node_find_index.call(null, this__19512.arr, this__19512.cnt, key);
  if(idx__19514 < 0) {
    return not_found
  }else {
    if(cljs.core.key_test.call(null, key, this__19512.arr[idx__19514])) {
      return this__19512.arr[idx__19514 + 1]
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
  var this__19515 = this;
  var inode__19516 = this;
  if(e === this__19515.edit) {
    this__19515.arr = array;
    this__19515.cnt = count;
    return inode__19516
  }else {
    return new cljs.core.HashCollisionNode(this__19515.edit, this__19515.collision_hash, count, array)
  }
};
cljs.core.HashCollisionNode;
cljs.core.create_node = function() {
  var create_node = null;
  var create_node__6 = function(shift, key1, val1, key2hash, key2, val2) {
    var key1hash__19521 = cljs.core.hash.call(null, key1);
    if(key1hash__19521 === key2hash) {
      return new cljs.core.HashCollisionNode(null, key1hash__19521, 2, [key1, val1, key2, val2])
    }else {
      var added_leaf_QMARK___19522 = new cljs.core.Box(false);
      return cljs.core.BitmapIndexedNode.EMPTY.inode_assoc(shift, key1hash__19521, key1, val1, added_leaf_QMARK___19522).inode_assoc(shift, key2hash, key2, val2, added_leaf_QMARK___19522)
    }
  };
  var create_node__7 = function(edit, shift, key1, val1, key2hash, key2, val2) {
    var key1hash__19523 = cljs.core.hash.call(null, key1);
    if(key1hash__19523 === key2hash) {
      return new cljs.core.HashCollisionNode(null, key1hash__19523, 2, [key1, val1, key2, val2])
    }else {
      var added_leaf_QMARK___19524 = new cljs.core.Box(false);
      return cljs.core.BitmapIndexedNode.EMPTY.inode_assoc_BANG_(edit, shift, key1hash__19523, key1, val1, added_leaf_QMARK___19524).inode_assoc_BANG_(edit, shift, key2hash, key2, val2, added_leaf_QMARK___19524)
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
  var this__19525 = this;
  var h__2247__auto____19526 = this__19525.__hash;
  if(!(h__2247__auto____19526 == null)) {
    return h__2247__auto____19526
  }else {
    var h__2247__auto____19527 = cljs.core.hash_coll.call(null, coll);
    this__19525.__hash = h__2247__auto____19527;
    return h__2247__auto____19527
  }
};
cljs.core.NodeSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19528 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.NodeSeq.prototype.toString = function() {
  var this__19529 = this;
  var this__19530 = this;
  return cljs.core.pr_str.call(null, this__19530)
};
cljs.core.NodeSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__19531 = this;
  return this$
};
cljs.core.NodeSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__19532 = this;
  if(this__19532.s == null) {
    return cljs.core.PersistentVector.fromArray([this__19532.nodes[this__19532.i], this__19532.nodes[this__19532.i + 1]], true)
  }else {
    return cljs.core.first.call(null, this__19532.s)
  }
};
cljs.core.NodeSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__19533 = this;
  if(this__19533.s == null) {
    return cljs.core.create_inode_seq.call(null, this__19533.nodes, this__19533.i + 2, null)
  }else {
    return cljs.core.create_inode_seq.call(null, this__19533.nodes, this__19533.i, cljs.core.next.call(null, this__19533.s))
  }
};
cljs.core.NodeSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19534 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.NodeSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19535 = this;
  return new cljs.core.NodeSeq(meta, this__19535.nodes, this__19535.i, this__19535.s, this__19535.__hash)
};
cljs.core.NodeSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19536 = this;
  return this__19536.meta
};
cljs.core.NodeSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19537 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19537.meta)
};
cljs.core.NodeSeq;
cljs.core.create_inode_seq = function() {
  var create_inode_seq = null;
  var create_inode_seq__1 = function(nodes) {
    return create_inode_seq.call(null, nodes, 0, null)
  };
  var create_inode_seq__3 = function(nodes, i, s) {
    if(s == null) {
      var len__19544 = nodes.length;
      var j__19545 = i;
      while(true) {
        if(j__19545 < len__19544) {
          if(!(nodes[j__19545] == null)) {
            return new cljs.core.NodeSeq(null, nodes, j__19545, null, null)
          }else {
            var temp__3971__auto____19546 = nodes[j__19545 + 1];
            if(cljs.core.truth_(temp__3971__auto____19546)) {
              var node__19547 = temp__3971__auto____19546;
              var temp__3971__auto____19548 = node__19547.inode_seq();
              if(cljs.core.truth_(temp__3971__auto____19548)) {
                var node_seq__19549 = temp__3971__auto____19548;
                return new cljs.core.NodeSeq(null, nodes, j__19545 + 2, node_seq__19549, null)
              }else {
                var G__19550 = j__19545 + 2;
                j__19545 = G__19550;
                continue
              }
            }else {
              var G__19551 = j__19545 + 2;
              j__19545 = G__19551;
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
  var this__19552 = this;
  var h__2247__auto____19553 = this__19552.__hash;
  if(!(h__2247__auto____19553 == null)) {
    return h__2247__auto____19553
  }else {
    var h__2247__auto____19554 = cljs.core.hash_coll.call(null, coll);
    this__19552.__hash = h__2247__auto____19554;
    return h__2247__auto____19554
  }
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19555 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.ArrayNodeSeq.prototype.toString = function() {
  var this__19556 = this;
  var this__19557 = this;
  return cljs.core.pr_str.call(null, this__19557)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__19558 = this;
  return this$
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(coll) {
  var this__19559 = this;
  return cljs.core.first.call(null, this__19559.s)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(coll) {
  var this__19560 = this;
  return cljs.core.create_array_node_seq.call(null, null, this__19560.nodes, this__19560.i, cljs.core.next.call(null, this__19560.s))
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19561 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19562 = this;
  return new cljs.core.ArrayNodeSeq(meta, this__19562.nodes, this__19562.i, this__19562.s, this__19562.__hash)
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19563 = this;
  return this__19563.meta
};
cljs.core.ArrayNodeSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19564 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19564.meta)
};
cljs.core.ArrayNodeSeq;
cljs.core.create_array_node_seq = function() {
  var create_array_node_seq = null;
  var create_array_node_seq__1 = function(nodes) {
    return create_array_node_seq.call(null, null, nodes, 0, null)
  };
  var create_array_node_seq__4 = function(meta, nodes, i, s) {
    if(s == null) {
      var len__19571 = nodes.length;
      var j__19572 = i;
      while(true) {
        if(j__19572 < len__19571) {
          var temp__3971__auto____19573 = nodes[j__19572];
          if(cljs.core.truth_(temp__3971__auto____19573)) {
            var nj__19574 = temp__3971__auto____19573;
            var temp__3971__auto____19575 = nj__19574.inode_seq();
            if(cljs.core.truth_(temp__3971__auto____19575)) {
              var ns__19576 = temp__3971__auto____19575;
              return new cljs.core.ArrayNodeSeq(meta, nodes, j__19572 + 1, ns__19576, null)
            }else {
              var G__19577 = j__19572 + 1;
              j__19572 = G__19577;
              continue
            }
          }else {
            var G__19578 = j__19572 + 1;
            j__19572 = G__19578;
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
  var this__19581 = this;
  return new cljs.core.TransientHashMap({}, this__19581.root, this__19581.cnt, this__19581.has_nil_QMARK_, this__19581.nil_val)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19582 = this;
  var h__2247__auto____19583 = this__19582.__hash;
  if(!(h__2247__auto____19583 == null)) {
    return h__2247__auto____19583
  }else {
    var h__2247__auto____19584 = cljs.core.hash_imap.call(null, coll);
    this__19582.__hash = h__2247__auto____19584;
    return h__2247__auto____19584
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19585 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.PersistentHashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19586 = this;
  if(k == null) {
    if(this__19586.has_nil_QMARK_) {
      return this__19586.nil_val
    }else {
      return not_found
    }
  }else {
    if(this__19586.root == null) {
      return not_found
    }else {
      if("\ufdd0'else") {
        return this__19586.root.inode_lookup(0, cljs.core.hash.call(null, k), k, not_found)
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19587 = this;
  if(k == null) {
    if(function() {
      var and__3822__auto____19588 = this__19587.has_nil_QMARK_;
      if(and__3822__auto____19588) {
        return v === this__19587.nil_val
      }else {
        return and__3822__auto____19588
      }
    }()) {
      return coll
    }else {
      return new cljs.core.PersistentHashMap(this__19587.meta, this__19587.has_nil_QMARK_ ? this__19587.cnt : this__19587.cnt + 1, this__19587.root, true, v, null)
    }
  }else {
    var added_leaf_QMARK___19589 = new cljs.core.Box(false);
    var new_root__19590 = (this__19587.root == null ? cljs.core.BitmapIndexedNode.EMPTY : this__19587.root).inode_assoc(0, cljs.core.hash.call(null, k), k, v, added_leaf_QMARK___19589);
    if(new_root__19590 === this__19587.root) {
      return coll
    }else {
      return new cljs.core.PersistentHashMap(this__19587.meta, added_leaf_QMARK___19589.val ? this__19587.cnt + 1 : this__19587.cnt, new_root__19590, this__19587.has_nil_QMARK_, this__19587.nil_val, null)
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19591 = this;
  if(k == null) {
    return this__19591.has_nil_QMARK_
  }else {
    if(this__19591.root == null) {
      return false
    }else {
      if("\ufdd0'else") {
        return!(this__19591.root.inode_lookup(0, cljs.core.hash.call(null, k), k, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel)
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.call = function() {
  var G__19614 = null;
  var G__19614__2 = function(this_sym19592, k) {
    var this__19594 = this;
    var this_sym19592__19595 = this;
    var coll__19596 = this_sym19592__19595;
    return coll__19596.cljs$core$ILookup$_lookup$arity$2(coll__19596, k)
  };
  var G__19614__3 = function(this_sym19593, k, not_found) {
    var this__19594 = this;
    var this_sym19593__19597 = this;
    var coll__19598 = this_sym19593__19597;
    return coll__19598.cljs$core$ILookup$_lookup$arity$3(coll__19598, k, not_found)
  };
  G__19614 = function(this_sym19593, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19614__2.call(this, this_sym19593, k);
      case 3:
        return G__19614__3.call(this, this_sym19593, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19614
}();
cljs.core.PersistentHashMap.prototype.apply = function(this_sym19579, args19580) {
  var this__19599 = this;
  return this_sym19579.call.apply(this_sym19579, [this_sym19579].concat(args19580.slice()))
};
cljs.core.PersistentHashMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(coll, f, init) {
  var this__19600 = this;
  var init__19601 = this__19600.has_nil_QMARK_ ? f.call(null, init, null, this__19600.nil_val) : init;
  if(cljs.core.reduced_QMARK_.call(null, init__19601)) {
    return cljs.core.deref.call(null, init__19601)
  }else {
    if(!(this__19600.root == null)) {
      return this__19600.root.kv_reduce(f, init__19601)
    }else {
      if("\ufdd0'else") {
        return init__19601
      }else {
        return null
      }
    }
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19602 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.PersistentHashMap.prototype.toString = function() {
  var this__19603 = this;
  var this__19604 = this;
  return cljs.core.pr_str.call(null, this__19604)
};
cljs.core.PersistentHashMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19605 = this;
  if(this__19605.cnt > 0) {
    var s__19606 = !(this__19605.root == null) ? this__19605.root.inode_seq() : null;
    if(this__19605.has_nil_QMARK_) {
      return cljs.core.cons.call(null, cljs.core.PersistentVector.fromArray([null, this__19605.nil_val], true), s__19606)
    }else {
      return s__19606
    }
  }else {
    return null
  }
};
cljs.core.PersistentHashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19607 = this;
  return this__19607.cnt
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19608 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19609 = this;
  return new cljs.core.PersistentHashMap(meta, this__19609.cnt, this__19609.root, this__19609.has_nil_QMARK_, this__19609.nil_val, this__19609.__hash)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19610 = this;
  return this__19610.meta
};
cljs.core.PersistentHashMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19611 = this;
  return cljs.core._with_meta.call(null, cljs.core.PersistentHashMap.EMPTY, this__19611.meta)
};
cljs.core.PersistentHashMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19612 = this;
  if(k == null) {
    if(this__19612.has_nil_QMARK_) {
      return new cljs.core.PersistentHashMap(this__19612.meta, this__19612.cnt - 1, this__19612.root, false, null, null)
    }else {
      return coll
    }
  }else {
    if(this__19612.root == null) {
      return coll
    }else {
      if("\ufdd0'else") {
        var new_root__19613 = this__19612.root.inode_without(0, cljs.core.hash.call(null, k), k);
        if(new_root__19613 === this__19612.root) {
          return coll
        }else {
          return new cljs.core.PersistentHashMap(this__19612.meta, this__19612.cnt - 1, new_root__19613, this__19612.has_nil_QMARK_, this__19612.nil_val, null)
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
  var len__19615 = ks.length;
  var i__19616 = 0;
  var out__19617 = cljs.core.transient$.call(null, cljs.core.PersistentHashMap.EMPTY);
  while(true) {
    if(i__19616 < len__19615) {
      var G__19618 = i__19616 + 1;
      var G__19619 = cljs.core.assoc_BANG_.call(null, out__19617, ks[i__19616], vs[i__19616]);
      i__19616 = G__19618;
      out__19617 = G__19619;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, out__19617)
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
  var this__19620 = this;
  return tcoll.without_BANG_(key)
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientAssociative$_assoc_BANG_$arity$3 = function(tcoll, key, val) {
  var this__19621 = this;
  return tcoll.assoc_BANG_(key, val)
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, val) {
  var this__19622 = this;
  return tcoll.conj_BANG_(val)
};
cljs.core.TransientHashMap.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__19623 = this;
  return tcoll.persistent_BANG_()
};
cljs.core.TransientHashMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(tcoll, k) {
  var this__19624 = this;
  if(k == null) {
    if(this__19624.has_nil_QMARK_) {
      return this__19624.nil_val
    }else {
      return null
    }
  }else {
    if(this__19624.root == null) {
      return null
    }else {
      return this__19624.root.inode_lookup(0, cljs.core.hash.call(null, k), k)
    }
  }
};
cljs.core.TransientHashMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(tcoll, k, not_found) {
  var this__19625 = this;
  if(k == null) {
    if(this__19625.has_nil_QMARK_) {
      return this__19625.nil_val
    }else {
      return not_found
    }
  }else {
    if(this__19625.root == null) {
      return not_found
    }else {
      return this__19625.root.inode_lookup(0, cljs.core.hash.call(null, k), k, not_found)
    }
  }
};
cljs.core.TransientHashMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19626 = this;
  if(this__19626.edit) {
    return this__19626.count
  }else {
    throw new Error("count after persistent!");
  }
};
cljs.core.TransientHashMap.prototype.conj_BANG_ = function(o) {
  var this__19627 = this;
  var tcoll__19628 = this;
  if(this__19627.edit) {
    if(function() {
      var G__19629__19630 = o;
      if(G__19629__19630) {
        if(function() {
          var or__3824__auto____19631 = G__19629__19630.cljs$lang$protocol_mask$partition0$ & 2048;
          if(or__3824__auto____19631) {
            return or__3824__auto____19631
          }else {
            return G__19629__19630.cljs$core$IMapEntry$
          }
        }()) {
          return true
        }else {
          if(!G__19629__19630.cljs$lang$protocol_mask$partition0$) {
            return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__19629__19630)
          }else {
            return false
          }
        }
      }else {
        return cljs.core.type_satisfies_.call(null, cljs.core.IMapEntry, G__19629__19630)
      }
    }()) {
      return tcoll__19628.assoc_BANG_(cljs.core.key.call(null, o), cljs.core.val.call(null, o))
    }else {
      var es__19632 = cljs.core.seq.call(null, o);
      var tcoll__19633 = tcoll__19628;
      while(true) {
        var temp__3971__auto____19634 = cljs.core.first.call(null, es__19632);
        if(cljs.core.truth_(temp__3971__auto____19634)) {
          var e__19635 = temp__3971__auto____19634;
          var G__19646 = cljs.core.next.call(null, es__19632);
          var G__19647 = tcoll__19633.assoc_BANG_(cljs.core.key.call(null, e__19635), cljs.core.val.call(null, e__19635));
          es__19632 = G__19646;
          tcoll__19633 = G__19647;
          continue
        }else {
          return tcoll__19633
        }
        break
      }
    }
  }else {
    throw new Error("conj! after persistent");
  }
};
cljs.core.TransientHashMap.prototype.assoc_BANG_ = function(k, v) {
  var this__19636 = this;
  var tcoll__19637 = this;
  if(this__19636.edit) {
    if(k == null) {
      if(this__19636.nil_val === v) {
      }else {
        this__19636.nil_val = v
      }
      if(this__19636.has_nil_QMARK_) {
      }else {
        this__19636.count = this__19636.count + 1;
        this__19636.has_nil_QMARK_ = true
      }
      return tcoll__19637
    }else {
      var added_leaf_QMARK___19638 = new cljs.core.Box(false);
      var node__19639 = (this__19636.root == null ? cljs.core.BitmapIndexedNode.EMPTY : this__19636.root).inode_assoc_BANG_(this__19636.edit, 0, cljs.core.hash.call(null, k), k, v, added_leaf_QMARK___19638);
      if(node__19639 === this__19636.root) {
      }else {
        this__19636.root = node__19639
      }
      if(added_leaf_QMARK___19638.val) {
        this__19636.count = this__19636.count + 1
      }else {
      }
      return tcoll__19637
    }
  }else {
    throw new Error("assoc! after persistent!");
  }
};
cljs.core.TransientHashMap.prototype.without_BANG_ = function(k) {
  var this__19640 = this;
  var tcoll__19641 = this;
  if(this__19640.edit) {
    if(k == null) {
      if(this__19640.has_nil_QMARK_) {
        this__19640.has_nil_QMARK_ = false;
        this__19640.nil_val = null;
        this__19640.count = this__19640.count - 1;
        return tcoll__19641
      }else {
        return tcoll__19641
      }
    }else {
      if(this__19640.root == null) {
        return tcoll__19641
      }else {
        var removed_leaf_QMARK___19642 = new cljs.core.Box(false);
        var node__19643 = this__19640.root.inode_without_BANG_(this__19640.edit, 0, cljs.core.hash.call(null, k), k, removed_leaf_QMARK___19642);
        if(node__19643 === this__19640.root) {
        }else {
          this__19640.root = node__19643
        }
        if(cljs.core.truth_(removed_leaf_QMARK___19642[0])) {
          this__19640.count = this__19640.count - 1
        }else {
        }
        return tcoll__19641
      }
    }
  }else {
    throw new Error("dissoc! after persistent!");
  }
};
cljs.core.TransientHashMap.prototype.persistent_BANG_ = function() {
  var this__19644 = this;
  var tcoll__19645 = this;
  if(this__19644.edit) {
    this__19644.edit = null;
    return new cljs.core.PersistentHashMap(null, this__19644.count, this__19644.root, this__19644.has_nil_QMARK_, this__19644.nil_val, null)
  }else {
    throw new Error("persistent! called twice");
  }
};
cljs.core.TransientHashMap;
cljs.core.tree_map_seq_push = function tree_map_seq_push(node, stack, ascending_QMARK_) {
  var t__19650 = node;
  var stack__19651 = stack;
  while(true) {
    if(!(t__19650 == null)) {
      var G__19652 = ascending_QMARK_ ? t__19650.left : t__19650.right;
      var G__19653 = cljs.core.conj.call(null, stack__19651, t__19650);
      t__19650 = G__19652;
      stack__19651 = G__19653;
      continue
    }else {
      return stack__19651
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
  var this__19654 = this;
  var h__2247__auto____19655 = this__19654.__hash;
  if(!(h__2247__auto____19655 == null)) {
    return h__2247__auto____19655
  }else {
    var h__2247__auto____19656 = cljs.core.hash_coll.call(null, coll);
    this__19654.__hash = h__2247__auto____19656;
    return h__2247__auto____19656
  }
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19657 = this;
  return cljs.core.cons.call(null, o, coll)
};
cljs.core.PersistentTreeMapSeq.prototype.toString = function() {
  var this__19658 = this;
  var this__19659 = this;
  return cljs.core.pr_str.call(null, this__19659)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeqable$_seq$arity$1 = function(this$) {
  var this__19660 = this;
  return this$
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19661 = this;
  if(this__19661.cnt < 0) {
    return cljs.core.count.call(null, cljs.core.next.call(null, coll)) + 1
  }else {
    return this__19661.cnt
  }
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeq$_first$arity$1 = function(this$) {
  var this__19662 = this;
  return cljs.core.peek.call(null, this__19662.stack)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$ISeq$_rest$arity$1 = function(this$) {
  var this__19663 = this;
  var t__19664 = cljs.core.first.call(null, this__19663.stack);
  var next_stack__19665 = cljs.core.tree_map_seq_push.call(null, this__19663.ascending_QMARK_ ? t__19664.right : t__19664.left, cljs.core.next.call(null, this__19663.stack), this__19663.ascending_QMARK_);
  if(!(next_stack__19665 == null)) {
    return new cljs.core.PersistentTreeMapSeq(null, next_stack__19665, this__19663.ascending_QMARK_, this__19663.cnt - 1, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19666 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19667 = this;
  return new cljs.core.PersistentTreeMapSeq(meta, this__19667.stack, this__19667.ascending_QMARK_, this__19667.cnt, this__19667.__hash)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19668 = this;
  return this__19668.meta
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19669 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__19669.meta)
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
        var and__3822__auto____19671 = cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, right);
        if(and__3822__auto____19671) {
          return cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, right.left)
        }else {
          return and__3822__auto____19671
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
        var and__3822__auto____19673 = cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, left);
        if(and__3822__auto____19673) {
          return cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, left.right)
        }else {
          return and__3822__auto____19673
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
  var init__19677 = f.call(null, init, node.key, node.val);
  if(cljs.core.reduced_QMARK_.call(null, init__19677)) {
    return cljs.core.deref.call(null, init__19677)
  }else {
    var init__19678 = !(node.left == null) ? tree_map_kv_reduce.call(null, node.left, f, init__19677) : init__19677;
    if(cljs.core.reduced_QMARK_.call(null, init__19678)) {
      return cljs.core.deref.call(null, init__19678)
    }else {
      var init__19679 = !(node.right == null) ? tree_map_kv_reduce.call(null, node.right, f, init__19678) : init__19678;
      if(cljs.core.reduced_QMARK_.call(null, init__19679)) {
        return cljs.core.deref.call(null, init__19679)
      }else {
        return init__19679
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
  var this__19682 = this;
  var h__2247__auto____19683 = this__19682.__hash;
  if(!(h__2247__auto____19683 == null)) {
    return h__2247__auto____19683
  }else {
    var h__2247__auto____19684 = cljs.core.hash_coll.call(null, coll);
    this__19682.__hash = h__2247__auto____19684;
    return h__2247__auto____19684
  }
};
cljs.core.BlackNode.prototype.cljs$core$ILookup$_lookup$arity$2 = function(node, k) {
  var this__19685 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, null)
};
cljs.core.BlackNode.prototype.cljs$core$ILookup$_lookup$arity$3 = function(node, k, not_found) {
  var this__19686 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, not_found)
};
cljs.core.BlackNode.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(node, k, v) {
  var this__19687 = this;
  return cljs.core.assoc.call(null, cljs.core.PersistentVector.fromArray([this__19687.key, this__19687.val], true), k, v)
};
cljs.core.BlackNode.prototype.call = function() {
  var G__19735 = null;
  var G__19735__2 = function(this_sym19688, k) {
    var this__19690 = this;
    var this_sym19688__19691 = this;
    var node__19692 = this_sym19688__19691;
    return node__19692.cljs$core$ILookup$_lookup$arity$2(node__19692, k)
  };
  var G__19735__3 = function(this_sym19689, k, not_found) {
    var this__19690 = this;
    var this_sym19689__19693 = this;
    var node__19694 = this_sym19689__19693;
    return node__19694.cljs$core$ILookup$_lookup$arity$3(node__19694, k, not_found)
  };
  G__19735 = function(this_sym19689, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19735__2.call(this, this_sym19689, k);
      case 3:
        return G__19735__3.call(this, this_sym19689, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19735
}();
cljs.core.BlackNode.prototype.apply = function(this_sym19680, args19681) {
  var this__19695 = this;
  return this_sym19680.call.apply(this_sym19680, [this_sym19680].concat(args19681.slice()))
};
cljs.core.BlackNode.prototype.cljs$core$ICollection$_conj$arity$2 = function(node, o) {
  var this__19696 = this;
  return cljs.core.PersistentVector.fromArray([this__19696.key, this__19696.val, o], true)
};
cljs.core.BlackNode.prototype.cljs$core$IMapEntry$_key$arity$1 = function(node) {
  var this__19697 = this;
  return this__19697.key
};
cljs.core.BlackNode.prototype.cljs$core$IMapEntry$_val$arity$1 = function(node) {
  var this__19698 = this;
  return this__19698.val
};
cljs.core.BlackNode.prototype.add_right = function(ins) {
  var this__19699 = this;
  var node__19700 = this;
  return ins.balance_right(node__19700)
};
cljs.core.BlackNode.prototype.redden = function() {
  var this__19701 = this;
  var node__19702 = this;
  return new cljs.core.RedNode(this__19701.key, this__19701.val, this__19701.left, this__19701.right, null)
};
cljs.core.BlackNode.prototype.remove_right = function(del) {
  var this__19703 = this;
  var node__19704 = this;
  return cljs.core.balance_right_del.call(null, this__19703.key, this__19703.val, this__19703.left, del)
};
cljs.core.BlackNode.prototype.replace = function(key, val, left, right) {
  var this__19705 = this;
  var node__19706 = this;
  return new cljs.core.BlackNode(key, val, left, right, null)
};
cljs.core.BlackNode.prototype.kv_reduce = function(f, init) {
  var this__19707 = this;
  var node__19708 = this;
  return cljs.core.tree_map_kv_reduce.call(null, node__19708, f, init)
};
cljs.core.BlackNode.prototype.remove_left = function(del) {
  var this__19709 = this;
  var node__19710 = this;
  return cljs.core.balance_left_del.call(null, this__19709.key, this__19709.val, del, this__19709.right)
};
cljs.core.BlackNode.prototype.add_left = function(ins) {
  var this__19711 = this;
  var node__19712 = this;
  return ins.balance_left(node__19712)
};
cljs.core.BlackNode.prototype.balance_left = function(parent) {
  var this__19713 = this;
  var node__19714 = this;
  return new cljs.core.BlackNode(parent.key, parent.val, node__19714, parent.right, null)
};
cljs.core.BlackNode.prototype.toString = function() {
  var G__19736 = null;
  var G__19736__0 = function() {
    var this__19715 = this;
    var this__19717 = this;
    return cljs.core.pr_str.call(null, this__19717)
  };
  G__19736 = function() {
    switch(arguments.length) {
      case 0:
        return G__19736__0.call(this)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19736
}();
cljs.core.BlackNode.prototype.balance_right = function(parent) {
  var this__19718 = this;
  var node__19719 = this;
  return new cljs.core.BlackNode(parent.key, parent.val, parent.left, node__19719, null)
};
cljs.core.BlackNode.prototype.blacken = function() {
  var this__19720 = this;
  var node__19721 = this;
  return node__19721
};
cljs.core.BlackNode.prototype.cljs$core$IReduce$_reduce$arity$2 = function(node, f) {
  var this__19722 = this;
  return cljs.core.ci_reduce.call(null, node, f)
};
cljs.core.BlackNode.prototype.cljs$core$IReduce$_reduce$arity$3 = function(node, f, start) {
  var this__19723 = this;
  return cljs.core.ci_reduce.call(null, node, f, start)
};
cljs.core.BlackNode.prototype.cljs$core$ISeqable$_seq$arity$1 = function(node) {
  var this__19724 = this;
  return cljs.core.list.call(null, this__19724.key, this__19724.val)
};
cljs.core.BlackNode.prototype.cljs$core$ICounted$_count$arity$1 = function(node) {
  var this__19725 = this;
  return 2
};
cljs.core.BlackNode.prototype.cljs$core$IStack$_peek$arity$1 = function(node) {
  var this__19726 = this;
  return this__19726.val
};
cljs.core.BlackNode.prototype.cljs$core$IStack$_pop$arity$1 = function(node) {
  var this__19727 = this;
  return cljs.core.PersistentVector.fromArray([this__19727.key], true)
};
cljs.core.BlackNode.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(node, n, v) {
  var this__19728 = this;
  return cljs.core._assoc_n.call(null, cljs.core.PersistentVector.fromArray([this__19728.key, this__19728.val], true), n, v)
};
cljs.core.BlackNode.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19729 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.BlackNode.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(node, meta) {
  var this__19730 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.fromArray([this__19730.key, this__19730.val], true), meta)
};
cljs.core.BlackNode.prototype.cljs$core$IMeta$_meta$arity$1 = function(node) {
  var this__19731 = this;
  return null
};
cljs.core.BlackNode.prototype.cljs$core$IIndexed$_nth$arity$2 = function(node, n) {
  var this__19732 = this;
  if(n === 0) {
    return this__19732.key
  }else {
    if(n === 1) {
      return this__19732.val
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
  var this__19733 = this;
  if(n === 0) {
    return this__19733.key
  }else {
    if(n === 1) {
      return this__19733.val
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
  var this__19734 = this;
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
  var this__19739 = this;
  var h__2247__auto____19740 = this__19739.__hash;
  if(!(h__2247__auto____19740 == null)) {
    return h__2247__auto____19740
  }else {
    var h__2247__auto____19741 = cljs.core.hash_coll.call(null, coll);
    this__19739.__hash = h__2247__auto____19741;
    return h__2247__auto____19741
  }
};
cljs.core.RedNode.prototype.cljs$core$ILookup$_lookup$arity$2 = function(node, k) {
  var this__19742 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, null)
};
cljs.core.RedNode.prototype.cljs$core$ILookup$_lookup$arity$3 = function(node, k, not_found) {
  var this__19743 = this;
  return node.cljs$core$IIndexed$_nth$arity$3(node, k, not_found)
};
cljs.core.RedNode.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(node, k, v) {
  var this__19744 = this;
  return cljs.core.assoc.call(null, cljs.core.PersistentVector.fromArray([this__19744.key, this__19744.val], true), k, v)
};
cljs.core.RedNode.prototype.call = function() {
  var G__19792 = null;
  var G__19792__2 = function(this_sym19745, k) {
    var this__19747 = this;
    var this_sym19745__19748 = this;
    var node__19749 = this_sym19745__19748;
    return node__19749.cljs$core$ILookup$_lookup$arity$2(node__19749, k)
  };
  var G__19792__3 = function(this_sym19746, k, not_found) {
    var this__19747 = this;
    var this_sym19746__19750 = this;
    var node__19751 = this_sym19746__19750;
    return node__19751.cljs$core$ILookup$_lookup$arity$3(node__19751, k, not_found)
  };
  G__19792 = function(this_sym19746, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19792__2.call(this, this_sym19746, k);
      case 3:
        return G__19792__3.call(this, this_sym19746, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19792
}();
cljs.core.RedNode.prototype.apply = function(this_sym19737, args19738) {
  var this__19752 = this;
  return this_sym19737.call.apply(this_sym19737, [this_sym19737].concat(args19738.slice()))
};
cljs.core.RedNode.prototype.cljs$core$ICollection$_conj$arity$2 = function(node, o) {
  var this__19753 = this;
  return cljs.core.PersistentVector.fromArray([this__19753.key, this__19753.val, o], true)
};
cljs.core.RedNode.prototype.cljs$core$IMapEntry$_key$arity$1 = function(node) {
  var this__19754 = this;
  return this__19754.key
};
cljs.core.RedNode.prototype.cljs$core$IMapEntry$_val$arity$1 = function(node) {
  var this__19755 = this;
  return this__19755.val
};
cljs.core.RedNode.prototype.add_right = function(ins) {
  var this__19756 = this;
  var node__19757 = this;
  return new cljs.core.RedNode(this__19756.key, this__19756.val, this__19756.left, ins, null)
};
cljs.core.RedNode.prototype.redden = function() {
  var this__19758 = this;
  var node__19759 = this;
  throw new Error("red-black tree invariant violation");
};
cljs.core.RedNode.prototype.remove_right = function(del) {
  var this__19760 = this;
  var node__19761 = this;
  return new cljs.core.RedNode(this__19760.key, this__19760.val, this__19760.left, del, null)
};
cljs.core.RedNode.prototype.replace = function(key, val, left, right) {
  var this__19762 = this;
  var node__19763 = this;
  return new cljs.core.RedNode(key, val, left, right, null)
};
cljs.core.RedNode.prototype.kv_reduce = function(f, init) {
  var this__19764 = this;
  var node__19765 = this;
  return cljs.core.tree_map_kv_reduce.call(null, node__19765, f, init)
};
cljs.core.RedNode.prototype.remove_left = function(del) {
  var this__19766 = this;
  var node__19767 = this;
  return new cljs.core.RedNode(this__19766.key, this__19766.val, del, this__19766.right, null)
};
cljs.core.RedNode.prototype.add_left = function(ins) {
  var this__19768 = this;
  var node__19769 = this;
  return new cljs.core.RedNode(this__19768.key, this__19768.val, ins, this__19768.right, null)
};
cljs.core.RedNode.prototype.balance_left = function(parent) {
  var this__19770 = this;
  var node__19771 = this;
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19770.left)) {
    return new cljs.core.RedNode(this__19770.key, this__19770.val, this__19770.left.blacken(), new cljs.core.BlackNode(parent.key, parent.val, this__19770.right, parent.right, null), null)
  }else {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19770.right)) {
      return new cljs.core.RedNode(this__19770.right.key, this__19770.right.val, new cljs.core.BlackNode(this__19770.key, this__19770.val, this__19770.left, this__19770.right.left, null), new cljs.core.BlackNode(parent.key, parent.val, this__19770.right.right, parent.right, null), null)
    }else {
      if("\ufdd0'else") {
        return new cljs.core.BlackNode(parent.key, parent.val, node__19771, parent.right, null)
      }else {
        return null
      }
    }
  }
};
cljs.core.RedNode.prototype.toString = function() {
  var G__19793 = null;
  var G__19793__0 = function() {
    var this__19772 = this;
    var this__19774 = this;
    return cljs.core.pr_str.call(null, this__19774)
  };
  G__19793 = function() {
    switch(arguments.length) {
      case 0:
        return G__19793__0.call(this)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19793
}();
cljs.core.RedNode.prototype.balance_right = function(parent) {
  var this__19775 = this;
  var node__19776 = this;
  if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19775.right)) {
    return new cljs.core.RedNode(this__19775.key, this__19775.val, new cljs.core.BlackNode(parent.key, parent.val, parent.left, this__19775.left, null), this__19775.right.blacken(), null)
  }else {
    if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, this__19775.left)) {
      return new cljs.core.RedNode(this__19775.left.key, this__19775.left.val, new cljs.core.BlackNode(parent.key, parent.val, parent.left, this__19775.left.left, null), new cljs.core.BlackNode(this__19775.key, this__19775.val, this__19775.left.right, this__19775.right, null), null)
    }else {
      if("\ufdd0'else") {
        return new cljs.core.BlackNode(parent.key, parent.val, parent.left, node__19776, null)
      }else {
        return null
      }
    }
  }
};
cljs.core.RedNode.prototype.blacken = function() {
  var this__19777 = this;
  var node__19778 = this;
  return new cljs.core.BlackNode(this__19777.key, this__19777.val, this__19777.left, this__19777.right, null)
};
cljs.core.RedNode.prototype.cljs$core$IReduce$_reduce$arity$2 = function(node, f) {
  var this__19779 = this;
  return cljs.core.ci_reduce.call(null, node, f)
};
cljs.core.RedNode.prototype.cljs$core$IReduce$_reduce$arity$3 = function(node, f, start) {
  var this__19780 = this;
  return cljs.core.ci_reduce.call(null, node, f, start)
};
cljs.core.RedNode.prototype.cljs$core$ISeqable$_seq$arity$1 = function(node) {
  var this__19781 = this;
  return cljs.core.list.call(null, this__19781.key, this__19781.val)
};
cljs.core.RedNode.prototype.cljs$core$ICounted$_count$arity$1 = function(node) {
  var this__19782 = this;
  return 2
};
cljs.core.RedNode.prototype.cljs$core$IStack$_peek$arity$1 = function(node) {
  var this__19783 = this;
  return this__19783.val
};
cljs.core.RedNode.prototype.cljs$core$IStack$_pop$arity$1 = function(node) {
  var this__19784 = this;
  return cljs.core.PersistentVector.fromArray([this__19784.key], true)
};
cljs.core.RedNode.prototype.cljs$core$IVector$_assoc_n$arity$3 = function(node, n, v) {
  var this__19785 = this;
  return cljs.core._assoc_n.call(null, cljs.core.PersistentVector.fromArray([this__19785.key, this__19785.val], true), n, v)
};
cljs.core.RedNode.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19786 = this;
  return cljs.core.equiv_sequential.call(null, coll, other)
};
cljs.core.RedNode.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(node, meta) {
  var this__19787 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentVector.fromArray([this__19787.key, this__19787.val], true), meta)
};
cljs.core.RedNode.prototype.cljs$core$IMeta$_meta$arity$1 = function(node) {
  var this__19788 = this;
  return null
};
cljs.core.RedNode.prototype.cljs$core$IIndexed$_nth$arity$2 = function(node, n) {
  var this__19789 = this;
  if(n === 0) {
    return this__19789.key
  }else {
    if(n === 1) {
      return this__19789.val
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
  var this__19790 = this;
  if(n === 0) {
    return this__19790.key
  }else {
    if(n === 1) {
      return this__19790.val
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
  var this__19791 = this;
  return cljs.core.PersistentVector.EMPTY
};
cljs.core.RedNode;
cljs.core.tree_map_add = function tree_map_add(comp, tree, k, v, found) {
  if(tree == null) {
    return new cljs.core.RedNode(k, v, null, null, null)
  }else {
    var c__19797 = comp.call(null, k, tree.key);
    if(c__19797 === 0) {
      found[0] = tree;
      return null
    }else {
      if(c__19797 < 0) {
        var ins__19798 = tree_map_add.call(null, comp, tree.left, k, v, found);
        if(!(ins__19798 == null)) {
          return tree.add_left(ins__19798)
        }else {
          return null
        }
      }else {
        if("\ufdd0'else") {
          var ins__19799 = tree_map_add.call(null, comp, tree.right, k, v, found);
          if(!(ins__19799 == null)) {
            return tree.add_right(ins__19799)
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
          var app__19802 = tree_map_append.call(null, left.right, right.left);
          if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, app__19802)) {
            return new cljs.core.RedNode(app__19802.key, app__19802.val, new cljs.core.RedNode(left.key, left.val, left.left, app__19802.left, null), new cljs.core.RedNode(right.key, right.val, app__19802.right, right.right, null), null)
          }else {
            return new cljs.core.RedNode(left.key, left.val, left.left, new cljs.core.RedNode(right.key, right.val, app__19802, right.right, null), null)
          }
        }else {
          return new cljs.core.RedNode(left.key, left.val, left.left, tree_map_append.call(null, left.right, right), null)
        }
      }else {
        if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, right)) {
          return new cljs.core.RedNode(right.key, right.val, tree_map_append.call(null, left, right.left), right.right, null)
        }else {
          if("\ufdd0'else") {
            var app__19803 = tree_map_append.call(null, left.right, right.left);
            if(cljs.core.instance_QMARK_.call(null, cljs.core.RedNode, app__19803)) {
              return new cljs.core.RedNode(app__19803.key, app__19803.val, new cljs.core.BlackNode(left.key, left.val, left.left, app__19803.left, null), new cljs.core.BlackNode(right.key, right.val, app__19803.right, right.right, null), null)
            }else {
              return cljs.core.balance_left_del.call(null, left.key, left.val, left.left, new cljs.core.BlackNode(right.key, right.val, app__19803, right.right, null))
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
    var c__19809 = comp.call(null, k, tree.key);
    if(c__19809 === 0) {
      found[0] = tree;
      return cljs.core.tree_map_append.call(null, tree.left, tree.right)
    }else {
      if(c__19809 < 0) {
        var del__19810 = tree_map_remove.call(null, comp, tree.left, k, found);
        if(function() {
          var or__3824__auto____19811 = !(del__19810 == null);
          if(or__3824__auto____19811) {
            return or__3824__auto____19811
          }else {
            return!(found[0] == null)
          }
        }()) {
          if(cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, tree.left)) {
            return cljs.core.balance_left_del.call(null, tree.key, tree.val, del__19810, tree.right)
          }else {
            return new cljs.core.RedNode(tree.key, tree.val, del__19810, tree.right, null)
          }
        }else {
          return null
        }
      }else {
        if("\ufdd0'else") {
          var del__19812 = tree_map_remove.call(null, comp, tree.right, k, found);
          if(function() {
            var or__3824__auto____19813 = !(del__19812 == null);
            if(or__3824__auto____19813) {
              return or__3824__auto____19813
            }else {
              return!(found[0] == null)
            }
          }()) {
            if(cljs.core.instance_QMARK_.call(null, cljs.core.BlackNode, tree.right)) {
              return cljs.core.balance_right_del.call(null, tree.key, tree.val, tree.left, del__19812)
            }else {
              return new cljs.core.RedNode(tree.key, tree.val, tree.left, del__19812, null)
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
  var tk__19816 = tree.key;
  var c__19817 = comp.call(null, k, tk__19816);
  if(c__19817 === 0) {
    return tree.replace(tk__19816, v, tree.left, tree.right)
  }else {
    if(c__19817 < 0) {
      return tree.replace(tk__19816, tree.val, tree_map_replace.call(null, comp, tree.left, k, v), tree.right)
    }else {
      if("\ufdd0'else") {
        return tree.replace(tk__19816, tree.val, tree.left, tree_map_replace.call(null, comp, tree.right, k, v))
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
  var this__19820 = this;
  var h__2247__auto____19821 = this__19820.__hash;
  if(!(h__2247__auto____19821 == null)) {
    return h__2247__auto____19821
  }else {
    var h__2247__auto____19822 = cljs.core.hash_imap.call(null, coll);
    this__19820.__hash = h__2247__auto____19822;
    return h__2247__auto____19822
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, k) {
  var this__19823 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, k, null)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, k, not_found) {
  var this__19824 = this;
  var n__19825 = coll.entry_at(k);
  if(!(n__19825 == null)) {
    return n__19825.val
  }else {
    return not_found
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IAssociative$_assoc$arity$3 = function(coll, k, v) {
  var this__19826 = this;
  var found__19827 = [null];
  var t__19828 = cljs.core.tree_map_add.call(null, this__19826.comp, this__19826.tree, k, v, found__19827);
  if(t__19828 == null) {
    var found_node__19829 = cljs.core.nth.call(null, found__19827, 0);
    if(cljs.core._EQ_.call(null, v, found_node__19829.val)) {
      return coll
    }else {
      return new cljs.core.PersistentTreeMap(this__19826.comp, cljs.core.tree_map_replace.call(null, this__19826.comp, this__19826.tree, k, v), this__19826.cnt, this__19826.meta, null)
    }
  }else {
    return new cljs.core.PersistentTreeMap(this__19826.comp, t__19828.blacken(), this__19826.cnt + 1, this__19826.meta, null)
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = function(coll, k) {
  var this__19830 = this;
  return!(coll.entry_at(k) == null)
};
cljs.core.PersistentTreeMap.prototype.call = function() {
  var G__19864 = null;
  var G__19864__2 = function(this_sym19831, k) {
    var this__19833 = this;
    var this_sym19831__19834 = this;
    var coll__19835 = this_sym19831__19834;
    return coll__19835.cljs$core$ILookup$_lookup$arity$2(coll__19835, k)
  };
  var G__19864__3 = function(this_sym19832, k, not_found) {
    var this__19833 = this;
    var this_sym19832__19836 = this;
    var coll__19837 = this_sym19832__19836;
    return coll__19837.cljs$core$ILookup$_lookup$arity$3(coll__19837, k, not_found)
  };
  G__19864 = function(this_sym19832, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19864__2.call(this, this_sym19832, k);
      case 3:
        return G__19864__3.call(this, this_sym19832, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19864
}();
cljs.core.PersistentTreeMap.prototype.apply = function(this_sym19818, args19819) {
  var this__19838 = this;
  return this_sym19818.call.apply(this_sym19818, [this_sym19818].concat(args19819.slice()))
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = function(coll, f, init) {
  var this__19839 = this;
  if(!(this__19839.tree == null)) {
    return cljs.core.tree_map_kv_reduce.call(null, this__19839.tree, f, init)
  }else {
    return init
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, entry) {
  var this__19840 = this;
  if(cljs.core.vector_QMARK_.call(null, entry)) {
    return coll.cljs$core$IAssociative$_assoc$arity$3(coll, cljs.core._nth.call(null, entry, 0), cljs.core._nth.call(null, entry, 1))
  }else {
    return cljs.core.reduce.call(null, cljs.core._conj, coll, entry)
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__19841 = this;
  if(this__19841.cnt > 0) {
    return cljs.core.create_tree_map_seq.call(null, this__19841.tree, false, this__19841.cnt)
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.toString = function() {
  var this__19842 = this;
  var this__19843 = this;
  return cljs.core.pr_str.call(null, this__19843)
};
cljs.core.PersistentTreeMap.prototype.entry_at = function(k) {
  var this__19844 = this;
  var coll__19845 = this;
  var t__19846 = this__19844.tree;
  while(true) {
    if(!(t__19846 == null)) {
      var c__19847 = this__19844.comp.call(null, k, t__19846.key);
      if(c__19847 === 0) {
        return t__19846
      }else {
        if(c__19847 < 0) {
          var G__19865 = t__19846.left;
          t__19846 = G__19865;
          continue
        }else {
          if("\ufdd0'else") {
            var G__19866 = t__19846.right;
            t__19846 = G__19866;
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
  var this__19848 = this;
  if(this__19848.cnt > 0) {
    return cljs.core.create_tree_map_seq.call(null, this__19848.tree, ascending_QMARK_, this__19848.cnt)
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_sorted_seq_from$arity$3 = function(coll, k, ascending_QMARK_) {
  var this__19849 = this;
  if(this__19849.cnt > 0) {
    var stack__19850 = null;
    var t__19851 = this__19849.tree;
    while(true) {
      if(!(t__19851 == null)) {
        var c__19852 = this__19849.comp.call(null, k, t__19851.key);
        if(c__19852 === 0) {
          return new cljs.core.PersistentTreeMapSeq(null, cljs.core.conj.call(null, stack__19850, t__19851), ascending_QMARK_, -1, null)
        }else {
          if(cljs.core.truth_(ascending_QMARK_)) {
            if(c__19852 < 0) {
              var G__19867 = cljs.core.conj.call(null, stack__19850, t__19851);
              var G__19868 = t__19851.left;
              stack__19850 = G__19867;
              t__19851 = G__19868;
              continue
            }else {
              var G__19869 = stack__19850;
              var G__19870 = t__19851.right;
              stack__19850 = G__19869;
              t__19851 = G__19870;
              continue
            }
          }else {
            if("\ufdd0'else") {
              if(c__19852 > 0) {
                var G__19871 = cljs.core.conj.call(null, stack__19850, t__19851);
                var G__19872 = t__19851.right;
                stack__19850 = G__19871;
                t__19851 = G__19872;
                continue
              }else {
                var G__19873 = stack__19850;
                var G__19874 = t__19851.left;
                stack__19850 = G__19873;
                t__19851 = G__19874;
                continue
              }
            }else {
              return null
            }
          }
        }
      }else {
        if(stack__19850 == null) {
          return new cljs.core.PersistentTreeMapSeq(null, stack__19850, ascending_QMARK_, -1, null)
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
  var this__19853 = this;
  return cljs.core.key.call(null, entry)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISorted$_comparator$arity$1 = function(coll) {
  var this__19854 = this;
  return this__19854.comp
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19855 = this;
  if(this__19855.cnt > 0) {
    return cljs.core.create_tree_map_seq.call(null, this__19855.tree, true, this__19855.cnt)
  }else {
    return null
  }
};
cljs.core.PersistentTreeMap.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19856 = this;
  return this__19856.cnt
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19857 = this;
  return cljs.core.equiv_map.call(null, coll, other)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19858 = this;
  return new cljs.core.PersistentTreeMap(this__19858.comp, this__19858.tree, this__19858.cnt, meta, this__19858.__hash)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19859 = this;
  return this__19859.meta
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19860 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentTreeMap.EMPTY, this__19860.meta)
};
cljs.core.PersistentTreeMap.prototype.cljs$core$IMap$_dissoc$arity$2 = function(coll, k) {
  var this__19861 = this;
  var found__19862 = [null];
  var t__19863 = cljs.core.tree_map_remove.call(null, this__19861.comp, this__19861.tree, k, found__19862);
  if(t__19863 == null) {
    if(cljs.core.nth.call(null, found__19862, 0) == null) {
      return coll
    }else {
      return new cljs.core.PersistentTreeMap(this__19861.comp, null, 0, this__19861.meta, null)
    }
  }else {
    return new cljs.core.PersistentTreeMap(this__19861.comp, t__19863.blacken(), this__19861.cnt - 1, this__19861.meta, null)
  }
};
cljs.core.PersistentTreeMap;
cljs.core.PersistentTreeMap.EMPTY = new cljs.core.PersistentTreeMap(cljs.core.compare, null, 0, null, 0);
cljs.core.hash_map = function() {
  var hash_map__delegate = function(keyvals) {
    var in__19877 = cljs.core.seq.call(null, keyvals);
    var out__19878 = cljs.core.transient$.call(null, cljs.core.PersistentHashMap.EMPTY);
    while(true) {
      if(in__19877) {
        var G__19879 = cljs.core.nnext.call(null, in__19877);
        var G__19880 = cljs.core.assoc_BANG_.call(null, out__19878, cljs.core.first.call(null, in__19877), cljs.core.second.call(null, in__19877));
        in__19877 = G__19879;
        out__19878 = G__19880;
        continue
      }else {
        return cljs.core.persistent_BANG_.call(null, out__19878)
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
  hash_map.cljs$lang$applyTo = function(arglist__19881) {
    var keyvals = cljs.core.seq(arglist__19881);
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
  array_map.cljs$lang$applyTo = function(arglist__19882) {
    var keyvals = cljs.core.seq(arglist__19882);
    return array_map__delegate(keyvals)
  };
  array_map.cljs$lang$arity$variadic = array_map__delegate;
  return array_map
}();
cljs.core.obj_map = function() {
  var obj_map__delegate = function(keyvals) {
    var ks__19886 = [];
    var obj__19887 = {};
    var kvs__19888 = cljs.core.seq.call(null, keyvals);
    while(true) {
      if(kvs__19888) {
        ks__19886.push(cljs.core.first.call(null, kvs__19888));
        obj__19887[cljs.core.first.call(null, kvs__19888)] = cljs.core.second.call(null, kvs__19888);
        var G__19889 = cljs.core.nnext.call(null, kvs__19888);
        kvs__19888 = G__19889;
        continue
      }else {
        return cljs.core.ObjMap.fromObject.call(null, ks__19886, obj__19887)
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
  obj_map.cljs$lang$applyTo = function(arglist__19890) {
    var keyvals = cljs.core.seq(arglist__19890);
    return obj_map__delegate(keyvals)
  };
  obj_map.cljs$lang$arity$variadic = obj_map__delegate;
  return obj_map
}();
cljs.core.sorted_map = function() {
  var sorted_map__delegate = function(keyvals) {
    var in__19893 = cljs.core.seq.call(null, keyvals);
    var out__19894 = cljs.core.PersistentTreeMap.EMPTY;
    while(true) {
      if(in__19893) {
        var G__19895 = cljs.core.nnext.call(null, in__19893);
        var G__19896 = cljs.core.assoc.call(null, out__19894, cljs.core.first.call(null, in__19893), cljs.core.second.call(null, in__19893));
        in__19893 = G__19895;
        out__19894 = G__19896;
        continue
      }else {
        return out__19894
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
  sorted_map.cljs$lang$applyTo = function(arglist__19897) {
    var keyvals = cljs.core.seq(arglist__19897);
    return sorted_map__delegate(keyvals)
  };
  sorted_map.cljs$lang$arity$variadic = sorted_map__delegate;
  return sorted_map
}();
cljs.core.sorted_map_by = function() {
  var sorted_map_by__delegate = function(comparator, keyvals) {
    var in__19900 = cljs.core.seq.call(null, keyvals);
    var out__19901 = new cljs.core.PersistentTreeMap(comparator, null, 0, null, 0);
    while(true) {
      if(in__19900) {
        var G__19902 = cljs.core.nnext.call(null, in__19900);
        var G__19903 = cljs.core.assoc.call(null, out__19901, cljs.core.first.call(null, in__19900), cljs.core.second.call(null, in__19900));
        in__19900 = G__19902;
        out__19901 = G__19903;
        continue
      }else {
        return out__19901
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
  sorted_map_by.cljs$lang$applyTo = function(arglist__19904) {
    var comparator = cljs.core.first(arglist__19904);
    var keyvals = cljs.core.rest(arglist__19904);
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
      return cljs.core.reduce.call(null, function(p1__19905_SHARP_, p2__19906_SHARP_) {
        return cljs.core.conj.call(null, function() {
          var or__3824__auto____19908 = p1__19905_SHARP_;
          if(cljs.core.truth_(or__3824__auto____19908)) {
            return or__3824__auto____19908
          }else {
            return cljs.core.ObjMap.EMPTY
          }
        }(), p2__19906_SHARP_)
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
  merge.cljs$lang$applyTo = function(arglist__19909) {
    var maps = cljs.core.seq(arglist__19909);
    return merge__delegate(maps)
  };
  merge.cljs$lang$arity$variadic = merge__delegate;
  return merge
}();
cljs.core.merge_with = function() {
  var merge_with__delegate = function(f, maps) {
    if(cljs.core.truth_(cljs.core.some.call(null, cljs.core.identity, maps))) {
      var merge_entry__19917 = function(m, e) {
        var k__19915 = cljs.core.first.call(null, e);
        var v__19916 = cljs.core.second.call(null, e);
        if(cljs.core.contains_QMARK_.call(null, m, k__19915)) {
          return cljs.core.assoc.call(null, m, k__19915, f.call(null, cljs.core._lookup.call(null, m, k__19915, null), v__19916))
        }else {
          return cljs.core.assoc.call(null, m, k__19915, v__19916)
        }
      };
      var merge2__19919 = function(m1, m2) {
        return cljs.core.reduce.call(null, merge_entry__19917, function() {
          var or__3824__auto____19918 = m1;
          if(cljs.core.truth_(or__3824__auto____19918)) {
            return or__3824__auto____19918
          }else {
            return cljs.core.ObjMap.EMPTY
          }
        }(), cljs.core.seq.call(null, m2))
      };
      return cljs.core.reduce.call(null, merge2__19919, maps)
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
  merge_with.cljs$lang$applyTo = function(arglist__19920) {
    var f = cljs.core.first(arglist__19920);
    var maps = cljs.core.rest(arglist__19920);
    return merge_with__delegate(f, maps)
  };
  merge_with.cljs$lang$arity$variadic = merge_with__delegate;
  return merge_with
}();
cljs.core.select_keys = function select_keys(map, keyseq) {
  var ret__19925 = cljs.core.ObjMap.EMPTY;
  var keys__19926 = cljs.core.seq.call(null, keyseq);
  while(true) {
    if(keys__19926) {
      var key__19927 = cljs.core.first.call(null, keys__19926);
      var entry__19928 = cljs.core._lookup.call(null, map, key__19927, "\ufdd0'cljs.core/not-found");
      var G__19929 = cljs.core.not_EQ_.call(null, entry__19928, "\ufdd0'cljs.core/not-found") ? cljs.core.assoc.call(null, ret__19925, key__19927, entry__19928) : ret__19925;
      var G__19930 = cljs.core.next.call(null, keys__19926);
      ret__19925 = G__19929;
      keys__19926 = G__19930;
      continue
    }else {
      return ret__19925
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
  var this__19934 = this;
  return new cljs.core.TransientHashSet(cljs.core.transient$.call(null, this__19934.hash_map))
};
cljs.core.PersistentHashSet.prototype.cljs$core$IHash$_hash$arity$1 = function(coll) {
  var this__19935 = this;
  var h__2247__auto____19936 = this__19935.__hash;
  if(!(h__2247__auto____19936 == null)) {
    return h__2247__auto____19936
  }else {
    var h__2247__auto____19937 = cljs.core.hash_iset.call(null, coll);
    this__19935.__hash = h__2247__auto____19937;
    return h__2247__auto____19937
  }
};
cljs.core.PersistentHashSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, v) {
  var this__19938 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, v, null)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, v, not_found) {
  var this__19939 = this;
  if(cljs.core.truth_(cljs.core._contains_key_QMARK_.call(null, this__19939.hash_map, v))) {
    return v
  }else {
    return not_found
  }
};
cljs.core.PersistentHashSet.prototype.call = function() {
  var G__19960 = null;
  var G__19960__2 = function(this_sym19940, k) {
    var this__19942 = this;
    var this_sym19940__19943 = this;
    var coll__19944 = this_sym19940__19943;
    return coll__19944.cljs$core$ILookup$_lookup$arity$2(coll__19944, k)
  };
  var G__19960__3 = function(this_sym19941, k, not_found) {
    var this__19942 = this;
    var this_sym19941__19945 = this;
    var coll__19946 = this_sym19941__19945;
    return coll__19946.cljs$core$ILookup$_lookup$arity$3(coll__19946, k, not_found)
  };
  G__19960 = function(this_sym19941, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19960__2.call(this, this_sym19941, k);
      case 3:
        return G__19960__3.call(this, this_sym19941, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19960
}();
cljs.core.PersistentHashSet.prototype.apply = function(this_sym19932, args19933) {
  var this__19947 = this;
  return this_sym19932.call.apply(this_sym19932, [this_sym19932].concat(args19933.slice()))
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19948 = this;
  return new cljs.core.PersistentHashSet(this__19948.meta, cljs.core.assoc.call(null, this__19948.hash_map, o, null), null)
};
cljs.core.PersistentHashSet.prototype.toString = function() {
  var this__19949 = this;
  var this__19950 = this;
  return cljs.core.pr_str.call(null, this__19950)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__19951 = this;
  return cljs.core.keys.call(null, this__19951.hash_map)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ISet$_disjoin$arity$2 = function(coll, v) {
  var this__19952 = this;
  return new cljs.core.PersistentHashSet(this__19952.meta, cljs.core.dissoc.call(null, this__19952.hash_map, v), null)
};
cljs.core.PersistentHashSet.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__19953 = this;
  return cljs.core.count.call(null, cljs.core.seq.call(null, coll))
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__19954 = this;
  var and__3822__auto____19955 = cljs.core.set_QMARK_.call(null, other);
  if(and__3822__auto____19955) {
    var and__3822__auto____19956 = cljs.core.count.call(null, coll) === cljs.core.count.call(null, other);
    if(and__3822__auto____19956) {
      return cljs.core.every_QMARK_.call(null, function(p1__19931_SHARP_) {
        return cljs.core.contains_QMARK_.call(null, coll, p1__19931_SHARP_)
      }, other)
    }else {
      return and__3822__auto____19956
    }
  }else {
    return and__3822__auto____19955
  }
};
cljs.core.PersistentHashSet.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__19957 = this;
  return new cljs.core.PersistentHashSet(meta, this__19957.hash_map, this__19957.__hash)
};
cljs.core.PersistentHashSet.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__19958 = this;
  return this__19958.meta
};
cljs.core.PersistentHashSet.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__19959 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentHashSet.EMPTY, this__19959.meta)
};
cljs.core.PersistentHashSet;
cljs.core.PersistentHashSet.EMPTY = new cljs.core.PersistentHashSet(null, cljs.core.hash_map.call(null), 0);
cljs.core.PersistentHashSet.fromArray = function(items) {
  var len__19961 = cljs.core.count.call(null, items);
  var i__19962 = 0;
  var out__19963 = cljs.core.transient$.call(null, cljs.core.PersistentHashSet.EMPTY);
  while(true) {
    if(i__19962 < len__19961) {
      var G__19964 = i__19962 + 1;
      var G__19965 = cljs.core.conj_BANG_.call(null, out__19963, items[i__19962]);
      i__19962 = G__19964;
      out__19963 = G__19965;
      continue
    }else {
      return cljs.core.persistent_BANG_.call(null, out__19963)
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
  var G__19983 = null;
  var G__19983__2 = function(this_sym19969, k) {
    var this__19971 = this;
    var this_sym19969__19972 = this;
    var tcoll__19973 = this_sym19969__19972;
    if(cljs.core._lookup.call(null, this__19971.transient_map, k, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
      return null
    }else {
      return k
    }
  };
  var G__19983__3 = function(this_sym19970, k, not_found) {
    var this__19971 = this;
    var this_sym19970__19974 = this;
    var tcoll__19975 = this_sym19970__19974;
    if(cljs.core._lookup.call(null, this__19971.transient_map, k, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
      return not_found
    }else {
      return k
    }
  };
  G__19983 = function(this_sym19970, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__19983__2.call(this, this_sym19970, k);
      case 3:
        return G__19983__3.call(this, this_sym19970, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__19983
}();
cljs.core.TransientHashSet.prototype.apply = function(this_sym19967, args19968) {
  var this__19976 = this;
  return this_sym19967.call.apply(this_sym19967, [this_sym19967].concat(args19968.slice()))
};
cljs.core.TransientHashSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(tcoll, v) {
  var this__19977 = this;
  return tcoll.cljs$core$ILookup$_lookup$arity$3(tcoll, v, null)
};
cljs.core.TransientHashSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(tcoll, v, not_found) {
  var this__19978 = this;
  if(cljs.core._lookup.call(null, this__19978.transient_map, v, cljs.core.lookup_sentinel) === cljs.core.lookup_sentinel) {
    return not_found
  }else {
    return v
  }
};
cljs.core.TransientHashSet.prototype.cljs$core$ICounted$_count$arity$1 = function(tcoll) {
  var this__19979 = this;
  return cljs.core.count.call(null, this__19979.transient_map)
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientSet$_disjoin_BANG_$arity$2 = function(tcoll, v) {
  var this__19980 = this;
  this__19980.transient_map = cljs.core.dissoc_BANG_.call(null, this__19980.transient_map, v);
  return tcoll
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientCollection$_conj_BANG_$arity$2 = function(tcoll, o) {
  var this__19981 = this;
  this__19981.transient_map = cljs.core.assoc_BANG_.call(null, this__19981.transient_map, o, null);
  return tcoll
};
cljs.core.TransientHashSet.prototype.cljs$core$ITransientCollection$_persistent_BANG_$arity$1 = function(tcoll) {
  var this__19982 = this;
  return new cljs.core.PersistentHashSet(null, cljs.core.persistent_BANG_.call(null, this__19982.transient_map), null)
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
  var this__19986 = this;
  var h__2247__auto____19987 = this__19986.__hash;
  if(!(h__2247__auto____19987 == null)) {
    return h__2247__auto____19987
  }else {
    var h__2247__auto____19988 = cljs.core.hash_iset.call(null, coll);
    this__19986.__hash = h__2247__auto____19988;
    return h__2247__auto____19988
  }
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ILookup$_lookup$arity$2 = function(coll, v) {
  var this__19989 = this;
  return coll.cljs$core$ILookup$_lookup$arity$3(coll, v, null)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ILookup$_lookup$arity$3 = function(coll, v, not_found) {
  var this__19990 = this;
  if(cljs.core.truth_(cljs.core._contains_key_QMARK_.call(null, this__19990.tree_map, v))) {
    return v
  }else {
    return not_found
  }
};
cljs.core.PersistentTreeSet.prototype.call = function() {
  var G__20016 = null;
  var G__20016__2 = function(this_sym19991, k) {
    var this__19993 = this;
    var this_sym19991__19994 = this;
    var coll__19995 = this_sym19991__19994;
    return coll__19995.cljs$core$ILookup$_lookup$arity$2(coll__19995, k)
  };
  var G__20016__3 = function(this_sym19992, k, not_found) {
    var this__19993 = this;
    var this_sym19992__19996 = this;
    var coll__19997 = this_sym19992__19996;
    return coll__19997.cljs$core$ILookup$_lookup$arity$3(coll__19997, k, not_found)
  };
  G__20016 = function(this_sym19992, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__20016__2.call(this, this_sym19992, k);
      case 3:
        return G__20016__3.call(this, this_sym19992, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__20016
}();
cljs.core.PersistentTreeSet.prototype.apply = function(this_sym19984, args19985) {
  var this__19998 = this;
  return this_sym19984.call.apply(this_sym19984, [this_sym19984].concat(args19985.slice()))
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICollection$_conj$arity$2 = function(coll, o) {
  var this__19999 = this;
  return new cljs.core.PersistentTreeSet(this__19999.meta, cljs.core.assoc.call(null, this__19999.tree_map, o, null), null)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IReversible$_rseq$arity$1 = function(coll) {
  var this__20000 = this;
  return cljs.core.map.call(null, cljs.core.key, cljs.core.rseq.call(null, this__20000.tree_map))
};
cljs.core.PersistentTreeSet.prototype.toString = function() {
  var this__20001 = this;
  var this__20002 = this;
  return cljs.core.pr_str.call(null, this__20002)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_sorted_seq$arity$2 = function(coll, ascending_QMARK_) {
  var this__20003 = this;
  return cljs.core.map.call(null, cljs.core.key, cljs.core._sorted_seq.call(null, this__20003.tree_map, ascending_QMARK_))
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_sorted_seq_from$arity$3 = function(coll, k, ascending_QMARK_) {
  var this__20004 = this;
  return cljs.core.map.call(null, cljs.core.key, cljs.core._sorted_seq_from.call(null, this__20004.tree_map, k, ascending_QMARK_))
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_entry_key$arity$2 = function(coll, entry) {
  var this__20005 = this;
  return entry
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISorted$_comparator$arity$1 = function(coll) {
  var this__20006 = this;
  return cljs.core._comparator.call(null, this__20006.tree_map)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISeqable$_seq$arity$1 = function(coll) {
  var this__20007 = this;
  return cljs.core.keys.call(null, this__20007.tree_map)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ISet$_disjoin$arity$2 = function(coll, v) {
  var this__20008 = this;
  return new cljs.core.PersistentTreeSet(this__20008.meta, cljs.core.dissoc.call(null, this__20008.tree_map, v), null)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$ICounted$_count$arity$1 = function(coll) {
  var this__20009 = this;
  return cljs.core.count.call(null, this__20009.tree_map)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(coll, other) {
  var this__20010 = this;
  var and__3822__auto____20011 = cljs.core.set_QMARK_.call(null, other);
  if(and__3822__auto____20011) {
    var and__3822__auto____20012 = cljs.core.count.call(null, coll) === cljs.core.count.call(null, other);
    if(and__3822__auto____20012) {
      return cljs.core.every_QMARK_.call(null, function(p1__19966_SHARP_) {
        return cljs.core.contains_QMARK_.call(null, coll, p1__19966_SHARP_)
      }, other)
    }else {
      return and__3822__auto____20012
    }
  }else {
    return and__3822__auto____20011
  }
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(coll, meta) {
  var this__20013 = this;
  return new cljs.core.PersistentTreeSet(meta, this__20013.tree_map, this__20013.__hash)
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IMeta$_meta$arity$1 = function(coll) {
  var this__20014 = this;
  return this__20014.meta
};
cljs.core.PersistentTreeSet.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(coll) {
  var this__20015 = this;
  return cljs.core.with_meta.call(null, cljs.core.PersistentTreeSet.EMPTY, this__20015.meta)
};
cljs.core.PersistentTreeSet;
cljs.core.PersistentTreeSet.EMPTY = new cljs.core.PersistentTreeSet(null, cljs.core.sorted_map.call(null), 0);
cljs.core.hash_set = function() {
  var hash_set = null;
  var hash_set__0 = function() {
    return cljs.core.PersistentHashSet.EMPTY
  };
  var hash_set__1 = function() {
    var G__20021__delegate = function(keys) {
      var in__20019 = cljs.core.seq.call(null, keys);
      var out__20020 = cljs.core.transient$.call(null, cljs.core.PersistentHashSet.EMPTY);
      while(true) {
        if(cljs.core.seq.call(null, in__20019)) {
          var G__20022 = cljs.core.next.call(null, in__20019);
          var G__20023 = cljs.core.conj_BANG_.call(null, out__20020, cljs.core.first.call(null, in__20019));
          in__20019 = G__20022;
          out__20020 = G__20023;
          continue
        }else {
          return cljs.core.persistent_BANG_.call(null, out__20020)
        }
        break
      }
    };
    var G__20021 = function(var_args) {
      var keys = null;
      if(goog.isDef(var_args)) {
        keys = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__20021__delegate.call(this, keys)
    };
    G__20021.cljs$lang$maxFixedArity = 0;
    G__20021.cljs$lang$applyTo = function(arglist__20024) {
      var keys = cljs.core.seq(arglist__20024);
      return G__20021__delegate(keys)
    };
    G__20021.cljs$lang$arity$variadic = G__20021__delegate;
    return G__20021
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
  sorted_set.cljs$lang$applyTo = function(arglist__20025) {
    var keys = cljs.core.seq(arglist__20025);
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
  sorted_set_by.cljs$lang$applyTo = function(arglist__20027) {
    var comparator = cljs.core.first(arglist__20027);
    var keys = cljs.core.rest(arglist__20027);
    return sorted_set_by__delegate(comparator, keys)
  };
  sorted_set_by.cljs$lang$arity$variadic = sorted_set_by__delegate;
  return sorted_set_by
}();
cljs.core.replace = function replace(smap, coll) {
  if(cljs.core.vector_QMARK_.call(null, coll)) {
    var n__20033 = cljs.core.count.call(null, coll);
    return cljs.core.reduce.call(null, function(v, i) {
      var temp__3971__auto____20034 = cljs.core.find.call(null, smap, cljs.core.nth.call(null, v, i));
      if(cljs.core.truth_(temp__3971__auto____20034)) {
        var e__20035 = temp__3971__auto____20034;
        return cljs.core.assoc.call(null, v, i, cljs.core.second.call(null, e__20035))
      }else {
        return v
      }
    }, coll, cljs.core.take.call(null, n__20033, cljs.core.iterate.call(null, cljs.core.inc, 0)))
  }else {
    return cljs.core.map.call(null, function(p1__20026_SHARP_) {
      var temp__3971__auto____20036 = cljs.core.find.call(null, smap, p1__20026_SHARP_);
      if(cljs.core.truth_(temp__3971__auto____20036)) {
        var e__20037 = temp__3971__auto____20036;
        return cljs.core.second.call(null, e__20037)
      }else {
        return p1__20026_SHARP_
      }
    }, coll)
  }
};
cljs.core.distinct = function distinct(coll) {
  var step__20067 = function step(xs, seen) {
    return new cljs.core.LazySeq(null, false, function() {
      return function(p__20060, seen) {
        while(true) {
          var vec__20061__20062 = p__20060;
          var f__20063 = cljs.core.nth.call(null, vec__20061__20062, 0, null);
          var xs__20064 = vec__20061__20062;
          var temp__3974__auto____20065 = cljs.core.seq.call(null, xs__20064);
          if(temp__3974__auto____20065) {
            var s__20066 = temp__3974__auto____20065;
            if(cljs.core.contains_QMARK_.call(null, seen, f__20063)) {
              var G__20068 = cljs.core.rest.call(null, s__20066);
              var G__20069 = seen;
              p__20060 = G__20068;
              seen = G__20069;
              continue
            }else {
              return cljs.core.cons.call(null, f__20063, step.call(null, cljs.core.rest.call(null, s__20066), cljs.core.conj.call(null, seen, f__20063)))
            }
          }else {
            return null
          }
          break
        }
      }.call(null, xs, seen)
    }, null)
  };
  return step__20067.call(null, coll, cljs.core.PersistentHashSet.EMPTY)
};
cljs.core.butlast = function butlast(s) {
  var ret__20072 = cljs.core.PersistentVector.EMPTY;
  var s__20073 = s;
  while(true) {
    if(cljs.core.next.call(null, s__20073)) {
      var G__20074 = cljs.core.conj.call(null, ret__20072, cljs.core.first.call(null, s__20073));
      var G__20075 = cljs.core.next.call(null, s__20073);
      ret__20072 = G__20074;
      s__20073 = G__20075;
      continue
    }else {
      return cljs.core.seq.call(null, ret__20072)
    }
    break
  }
};
cljs.core.name = function name(x) {
  if(cljs.core.string_QMARK_.call(null, x)) {
    return x
  }else {
    if(function() {
      var or__3824__auto____20078 = cljs.core.keyword_QMARK_.call(null, x);
      if(or__3824__auto____20078) {
        return or__3824__auto____20078
      }else {
        return cljs.core.symbol_QMARK_.call(null, x)
      }
    }()) {
      var i__20079 = x.lastIndexOf("/");
      if(i__20079 < 0) {
        return cljs.core.subs.call(null, x, 2)
      }else {
        return cljs.core.subs.call(null, x, i__20079 + 1)
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
    var or__3824__auto____20082 = cljs.core.keyword_QMARK_.call(null, x);
    if(or__3824__auto____20082) {
      return or__3824__auto____20082
    }else {
      return cljs.core.symbol_QMARK_.call(null, x)
    }
  }()) {
    var i__20083 = x.lastIndexOf("/");
    if(i__20083 > -1) {
      return cljs.core.subs.call(null, x, 2, i__20083)
    }else {
      return null
    }
  }else {
    throw new Error([cljs.core.str("Doesn't support namespace: "), cljs.core.str(x)].join(""));
  }
};
cljs.core.zipmap = function zipmap(keys, vals) {
  var map__20090 = cljs.core.ObjMap.EMPTY;
  var ks__20091 = cljs.core.seq.call(null, keys);
  var vs__20092 = cljs.core.seq.call(null, vals);
  while(true) {
    if(function() {
      var and__3822__auto____20093 = ks__20091;
      if(and__3822__auto____20093) {
        return vs__20092
      }else {
        return and__3822__auto____20093
      }
    }()) {
      var G__20094 = cljs.core.assoc.call(null, map__20090, cljs.core.first.call(null, ks__20091), cljs.core.first.call(null, vs__20092));
      var G__20095 = cljs.core.next.call(null, ks__20091);
      var G__20096 = cljs.core.next.call(null, vs__20092);
      map__20090 = G__20094;
      ks__20091 = G__20095;
      vs__20092 = G__20096;
      continue
    }else {
      return map__20090
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
    var G__20099__delegate = function(k, x, y, more) {
      return cljs.core.reduce.call(null, function(p1__20084_SHARP_, p2__20085_SHARP_) {
        return max_key.call(null, k, p1__20084_SHARP_, p2__20085_SHARP_)
      }, max_key.call(null, k, x, y), more)
    };
    var G__20099 = function(k, x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__20099__delegate.call(this, k, x, y, more)
    };
    G__20099.cljs$lang$maxFixedArity = 3;
    G__20099.cljs$lang$applyTo = function(arglist__20100) {
      var k = cljs.core.first(arglist__20100);
      var x = cljs.core.first(cljs.core.next(arglist__20100));
      var y = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20100)));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20100)));
      return G__20099__delegate(k, x, y, more)
    };
    G__20099.cljs$lang$arity$variadic = G__20099__delegate;
    return G__20099
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
    var G__20101__delegate = function(k, x, y, more) {
      return cljs.core.reduce.call(null, function(p1__20097_SHARP_, p2__20098_SHARP_) {
        return min_key.call(null, k, p1__20097_SHARP_, p2__20098_SHARP_)
      }, min_key.call(null, k, x, y), more)
    };
    var G__20101 = function(k, x, y, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__20101__delegate.call(this, k, x, y, more)
    };
    G__20101.cljs$lang$maxFixedArity = 3;
    G__20101.cljs$lang$applyTo = function(arglist__20102) {
      var k = cljs.core.first(arglist__20102);
      var x = cljs.core.first(cljs.core.next(arglist__20102));
      var y = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20102)));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20102)));
      return G__20101__delegate(k, x, y, more)
    };
    G__20101.cljs$lang$arity$variadic = G__20101__delegate;
    return G__20101
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
      var temp__3974__auto____20105 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____20105) {
        var s__20106 = temp__3974__auto____20105;
        return cljs.core.cons.call(null, cljs.core.take.call(null, n, s__20106), partition_all.call(null, n, step, cljs.core.drop.call(null, step, s__20106)))
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
    var temp__3974__auto____20109 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____20109) {
      var s__20110 = temp__3974__auto____20109;
      if(cljs.core.truth_(pred.call(null, cljs.core.first.call(null, s__20110)))) {
        return cljs.core.cons.call(null, cljs.core.first.call(null, s__20110), take_while.call(null, pred, cljs.core.rest.call(null, s__20110)))
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
    var comp__20112 = cljs.core._comparator.call(null, sc);
    return test.call(null, comp__20112.call(null, cljs.core._entry_key.call(null, sc, e), key), 0)
  }
};
cljs.core.subseq = function() {
  var subseq = null;
  var subseq__3 = function(sc, test, key) {
    var include__20124 = cljs.core.mk_bound_fn.call(null, sc, test, key);
    if(cljs.core.truth_(cljs.core.PersistentHashSet.fromArray([cljs.core._GT_, cljs.core._GT__EQ_]).call(null, test))) {
      var temp__3974__auto____20125 = cljs.core._sorted_seq_from.call(null, sc, key, true);
      if(cljs.core.truth_(temp__3974__auto____20125)) {
        var vec__20126__20127 = temp__3974__auto____20125;
        var e__20128 = cljs.core.nth.call(null, vec__20126__20127, 0, null);
        var s__20129 = vec__20126__20127;
        if(cljs.core.truth_(include__20124.call(null, e__20128))) {
          return s__20129
        }else {
          return cljs.core.next.call(null, s__20129)
        }
      }else {
        return null
      }
    }else {
      return cljs.core.take_while.call(null, include__20124, cljs.core._sorted_seq.call(null, sc, true))
    }
  };
  var subseq__5 = function(sc, start_test, start_key, end_test, end_key) {
    var temp__3974__auto____20130 = cljs.core._sorted_seq_from.call(null, sc, start_key, true);
    if(cljs.core.truth_(temp__3974__auto____20130)) {
      var vec__20131__20132 = temp__3974__auto____20130;
      var e__20133 = cljs.core.nth.call(null, vec__20131__20132, 0, null);
      var s__20134 = vec__20131__20132;
      return cljs.core.take_while.call(null, cljs.core.mk_bound_fn.call(null, sc, end_test, end_key), cljs.core.truth_(cljs.core.mk_bound_fn.call(null, sc, start_test, start_key).call(null, e__20133)) ? s__20134 : cljs.core.next.call(null, s__20134))
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
    var include__20146 = cljs.core.mk_bound_fn.call(null, sc, test, key);
    if(cljs.core.truth_(cljs.core.PersistentHashSet.fromArray([cljs.core._LT_, cljs.core._LT__EQ_]).call(null, test))) {
      var temp__3974__auto____20147 = cljs.core._sorted_seq_from.call(null, sc, key, false);
      if(cljs.core.truth_(temp__3974__auto____20147)) {
        var vec__20148__20149 = temp__3974__auto____20147;
        var e__20150 = cljs.core.nth.call(null, vec__20148__20149, 0, null);
        var s__20151 = vec__20148__20149;
        if(cljs.core.truth_(include__20146.call(null, e__20150))) {
          return s__20151
        }else {
          return cljs.core.next.call(null, s__20151)
        }
      }else {
        return null
      }
    }else {
      return cljs.core.take_while.call(null, include__20146, cljs.core._sorted_seq.call(null, sc, false))
    }
  };
  var rsubseq__5 = function(sc, start_test, start_key, end_test, end_key) {
    var temp__3974__auto____20152 = cljs.core._sorted_seq_from.call(null, sc, end_key, false);
    if(cljs.core.truth_(temp__3974__auto____20152)) {
      var vec__20153__20154 = temp__3974__auto____20152;
      var e__20155 = cljs.core.nth.call(null, vec__20153__20154, 0, null);
      var s__20156 = vec__20153__20154;
      return cljs.core.take_while.call(null, cljs.core.mk_bound_fn.call(null, sc, start_test, start_key), cljs.core.truth_(cljs.core.mk_bound_fn.call(null, sc, end_test, end_key).call(null, e__20155)) ? s__20156 : cljs.core.next.call(null, s__20156))
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
  var this__20157 = this;
  var h__2247__auto____20158 = this__20157.__hash;
  if(!(h__2247__auto____20158 == null)) {
    return h__2247__auto____20158
  }else {
    var h__2247__auto____20159 = cljs.core.hash_coll.call(null, rng);
    this__20157.__hash = h__2247__auto____20159;
    return h__2247__auto____20159
  }
};
cljs.core.Range.prototype.cljs$core$INext$_next$arity$1 = function(rng) {
  var this__20160 = this;
  if(this__20160.step > 0) {
    if(this__20160.start + this__20160.step < this__20160.end) {
      return new cljs.core.Range(this__20160.meta, this__20160.start + this__20160.step, this__20160.end, this__20160.step, null)
    }else {
      return null
    }
  }else {
    if(this__20160.start + this__20160.step > this__20160.end) {
      return new cljs.core.Range(this__20160.meta, this__20160.start + this__20160.step, this__20160.end, this__20160.step, null)
    }else {
      return null
    }
  }
};
cljs.core.Range.prototype.cljs$core$ICollection$_conj$arity$2 = function(rng, o) {
  var this__20161 = this;
  return cljs.core.cons.call(null, o, rng)
};
cljs.core.Range.prototype.toString = function() {
  var this__20162 = this;
  var this__20163 = this;
  return cljs.core.pr_str.call(null, this__20163)
};
cljs.core.Range.prototype.cljs$core$IReduce$_reduce$arity$2 = function(rng, f) {
  var this__20164 = this;
  return cljs.core.ci_reduce.call(null, rng, f)
};
cljs.core.Range.prototype.cljs$core$IReduce$_reduce$arity$3 = function(rng, f, s) {
  var this__20165 = this;
  return cljs.core.ci_reduce.call(null, rng, f, s)
};
cljs.core.Range.prototype.cljs$core$ISeqable$_seq$arity$1 = function(rng) {
  var this__20166 = this;
  if(this__20166.step > 0) {
    if(this__20166.start < this__20166.end) {
      return rng
    }else {
      return null
    }
  }else {
    if(this__20166.start > this__20166.end) {
      return rng
    }else {
      return null
    }
  }
};
cljs.core.Range.prototype.cljs$core$ICounted$_count$arity$1 = function(rng) {
  var this__20167 = this;
  if(cljs.core.not.call(null, rng.cljs$core$ISeqable$_seq$arity$1(rng))) {
    return 0
  }else {
    return Math.ceil((this__20167.end - this__20167.start) / this__20167.step)
  }
};
cljs.core.Range.prototype.cljs$core$ISeq$_first$arity$1 = function(rng) {
  var this__20168 = this;
  return this__20168.start
};
cljs.core.Range.prototype.cljs$core$ISeq$_rest$arity$1 = function(rng) {
  var this__20169 = this;
  if(!(rng.cljs$core$ISeqable$_seq$arity$1(rng) == null)) {
    return new cljs.core.Range(this__20169.meta, this__20169.start + this__20169.step, this__20169.end, this__20169.step, null)
  }else {
    return cljs.core.List.EMPTY
  }
};
cljs.core.Range.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(rng, other) {
  var this__20170 = this;
  return cljs.core.equiv_sequential.call(null, rng, other)
};
cljs.core.Range.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = function(rng, meta) {
  var this__20171 = this;
  return new cljs.core.Range(meta, this__20171.start, this__20171.end, this__20171.step, this__20171.__hash)
};
cljs.core.Range.prototype.cljs$core$IMeta$_meta$arity$1 = function(rng) {
  var this__20172 = this;
  return this__20172.meta
};
cljs.core.Range.prototype.cljs$core$IIndexed$_nth$arity$2 = function(rng, n) {
  var this__20173 = this;
  if(n < rng.cljs$core$ICounted$_count$arity$1(rng)) {
    return this__20173.start + n * this__20173.step
  }else {
    if(function() {
      var and__3822__auto____20174 = this__20173.start > this__20173.end;
      if(and__3822__auto____20174) {
        return this__20173.step === 0
      }else {
        return and__3822__auto____20174
      }
    }()) {
      return this__20173.start
    }else {
      throw new Error("Index out of bounds");
    }
  }
};
cljs.core.Range.prototype.cljs$core$IIndexed$_nth$arity$3 = function(rng, n, not_found) {
  var this__20175 = this;
  if(n < rng.cljs$core$ICounted$_count$arity$1(rng)) {
    return this__20175.start + n * this__20175.step
  }else {
    if(function() {
      var and__3822__auto____20176 = this__20175.start > this__20175.end;
      if(and__3822__auto____20176) {
        return this__20175.step === 0
      }else {
        return and__3822__auto____20176
      }
    }()) {
      return this__20175.start
    }else {
      return not_found
    }
  }
};
cljs.core.Range.prototype.cljs$core$IEmptyableCollection$_empty$arity$1 = function(rng) {
  var this__20177 = this;
  return cljs.core.with_meta.call(null, cljs.core.List.EMPTY, this__20177.meta)
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
    var temp__3974__auto____20180 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____20180) {
      var s__20181 = temp__3974__auto____20180;
      return cljs.core.cons.call(null, cljs.core.first.call(null, s__20181), take_nth.call(null, n, cljs.core.drop.call(null, n, s__20181)))
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
    var temp__3974__auto____20188 = cljs.core.seq.call(null, coll);
    if(temp__3974__auto____20188) {
      var s__20189 = temp__3974__auto____20188;
      var fst__20190 = cljs.core.first.call(null, s__20189);
      var fv__20191 = f.call(null, fst__20190);
      var run__20192 = cljs.core.cons.call(null, fst__20190, cljs.core.take_while.call(null, function(p1__20182_SHARP_) {
        return cljs.core._EQ_.call(null, fv__20191, f.call(null, p1__20182_SHARP_))
      }, cljs.core.next.call(null, s__20189)));
      return cljs.core.cons.call(null, run__20192, partition_by.call(null, f, cljs.core.seq.call(null, cljs.core.drop.call(null, cljs.core.count.call(null, run__20192), s__20189))))
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
      var temp__3971__auto____20207 = cljs.core.seq.call(null, coll);
      if(temp__3971__auto____20207) {
        var s__20208 = temp__3971__auto____20207;
        return reductions.call(null, f, cljs.core.first.call(null, s__20208), cljs.core.rest.call(null, s__20208))
      }else {
        return cljs.core.list.call(null, f.call(null))
      }
    }, null)
  };
  var reductions__3 = function(f, init, coll) {
    return cljs.core.cons.call(null, init, new cljs.core.LazySeq(null, false, function() {
      var temp__3974__auto____20209 = cljs.core.seq.call(null, coll);
      if(temp__3974__auto____20209) {
        var s__20210 = temp__3974__auto____20209;
        return reductions.call(null, f, f.call(null, init, cljs.core.first.call(null, s__20210)), cljs.core.rest.call(null, s__20210))
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
      var G__20213 = null;
      var G__20213__0 = function() {
        return cljs.core.vector.call(null, f.call(null))
      };
      var G__20213__1 = function(x) {
        return cljs.core.vector.call(null, f.call(null, x))
      };
      var G__20213__2 = function(x, y) {
        return cljs.core.vector.call(null, f.call(null, x, y))
      };
      var G__20213__3 = function(x, y, z) {
        return cljs.core.vector.call(null, f.call(null, x, y, z))
      };
      var G__20213__4 = function() {
        var G__20214__delegate = function(x, y, z, args) {
          return cljs.core.vector.call(null, cljs.core.apply.call(null, f, x, y, z, args))
        };
        var G__20214 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__20214__delegate.call(this, x, y, z, args)
        };
        G__20214.cljs$lang$maxFixedArity = 3;
        G__20214.cljs$lang$applyTo = function(arglist__20215) {
          var x = cljs.core.first(arglist__20215);
          var y = cljs.core.first(cljs.core.next(arglist__20215));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20215)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20215)));
          return G__20214__delegate(x, y, z, args)
        };
        G__20214.cljs$lang$arity$variadic = G__20214__delegate;
        return G__20214
      }();
      G__20213 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__20213__0.call(this);
          case 1:
            return G__20213__1.call(this, x);
          case 2:
            return G__20213__2.call(this, x, y);
          case 3:
            return G__20213__3.call(this, x, y, z);
          default:
            return G__20213__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__20213.cljs$lang$maxFixedArity = 3;
      G__20213.cljs$lang$applyTo = G__20213__4.cljs$lang$applyTo;
      return G__20213
    }()
  };
  var juxt__2 = function(f, g) {
    return function() {
      var G__20216 = null;
      var G__20216__0 = function() {
        return cljs.core.vector.call(null, f.call(null), g.call(null))
      };
      var G__20216__1 = function(x) {
        return cljs.core.vector.call(null, f.call(null, x), g.call(null, x))
      };
      var G__20216__2 = function(x, y) {
        return cljs.core.vector.call(null, f.call(null, x, y), g.call(null, x, y))
      };
      var G__20216__3 = function(x, y, z) {
        return cljs.core.vector.call(null, f.call(null, x, y, z), g.call(null, x, y, z))
      };
      var G__20216__4 = function() {
        var G__20217__delegate = function(x, y, z, args) {
          return cljs.core.vector.call(null, cljs.core.apply.call(null, f, x, y, z, args), cljs.core.apply.call(null, g, x, y, z, args))
        };
        var G__20217 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__20217__delegate.call(this, x, y, z, args)
        };
        G__20217.cljs$lang$maxFixedArity = 3;
        G__20217.cljs$lang$applyTo = function(arglist__20218) {
          var x = cljs.core.first(arglist__20218);
          var y = cljs.core.first(cljs.core.next(arglist__20218));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20218)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20218)));
          return G__20217__delegate(x, y, z, args)
        };
        G__20217.cljs$lang$arity$variadic = G__20217__delegate;
        return G__20217
      }();
      G__20216 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__20216__0.call(this);
          case 1:
            return G__20216__1.call(this, x);
          case 2:
            return G__20216__2.call(this, x, y);
          case 3:
            return G__20216__3.call(this, x, y, z);
          default:
            return G__20216__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__20216.cljs$lang$maxFixedArity = 3;
      G__20216.cljs$lang$applyTo = G__20216__4.cljs$lang$applyTo;
      return G__20216
    }()
  };
  var juxt__3 = function(f, g, h) {
    return function() {
      var G__20219 = null;
      var G__20219__0 = function() {
        return cljs.core.vector.call(null, f.call(null), g.call(null), h.call(null))
      };
      var G__20219__1 = function(x) {
        return cljs.core.vector.call(null, f.call(null, x), g.call(null, x), h.call(null, x))
      };
      var G__20219__2 = function(x, y) {
        return cljs.core.vector.call(null, f.call(null, x, y), g.call(null, x, y), h.call(null, x, y))
      };
      var G__20219__3 = function(x, y, z) {
        return cljs.core.vector.call(null, f.call(null, x, y, z), g.call(null, x, y, z), h.call(null, x, y, z))
      };
      var G__20219__4 = function() {
        var G__20220__delegate = function(x, y, z, args) {
          return cljs.core.vector.call(null, cljs.core.apply.call(null, f, x, y, z, args), cljs.core.apply.call(null, g, x, y, z, args), cljs.core.apply.call(null, h, x, y, z, args))
        };
        var G__20220 = function(x, y, z, var_args) {
          var args = null;
          if(goog.isDef(var_args)) {
            args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
          }
          return G__20220__delegate.call(this, x, y, z, args)
        };
        G__20220.cljs$lang$maxFixedArity = 3;
        G__20220.cljs$lang$applyTo = function(arglist__20221) {
          var x = cljs.core.first(arglist__20221);
          var y = cljs.core.first(cljs.core.next(arglist__20221));
          var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20221)));
          var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20221)));
          return G__20220__delegate(x, y, z, args)
        };
        G__20220.cljs$lang$arity$variadic = G__20220__delegate;
        return G__20220
      }();
      G__20219 = function(x, y, z, var_args) {
        var args = var_args;
        switch(arguments.length) {
          case 0:
            return G__20219__0.call(this);
          case 1:
            return G__20219__1.call(this, x);
          case 2:
            return G__20219__2.call(this, x, y);
          case 3:
            return G__20219__3.call(this, x, y, z);
          default:
            return G__20219__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
        }
        throw"Invalid arity: " + arguments.length;
      };
      G__20219.cljs$lang$maxFixedArity = 3;
      G__20219.cljs$lang$applyTo = G__20219__4.cljs$lang$applyTo;
      return G__20219
    }()
  };
  var juxt__4 = function() {
    var G__20222__delegate = function(f, g, h, fs) {
      var fs__20212 = cljs.core.list_STAR_.call(null, f, g, h, fs);
      return function() {
        var G__20223 = null;
        var G__20223__0 = function() {
          return cljs.core.reduce.call(null, function(p1__20193_SHARP_, p2__20194_SHARP_) {
            return cljs.core.conj.call(null, p1__20193_SHARP_, p2__20194_SHARP_.call(null))
          }, cljs.core.PersistentVector.EMPTY, fs__20212)
        };
        var G__20223__1 = function(x) {
          return cljs.core.reduce.call(null, function(p1__20195_SHARP_, p2__20196_SHARP_) {
            return cljs.core.conj.call(null, p1__20195_SHARP_, p2__20196_SHARP_.call(null, x))
          }, cljs.core.PersistentVector.EMPTY, fs__20212)
        };
        var G__20223__2 = function(x, y) {
          return cljs.core.reduce.call(null, function(p1__20197_SHARP_, p2__20198_SHARP_) {
            return cljs.core.conj.call(null, p1__20197_SHARP_, p2__20198_SHARP_.call(null, x, y))
          }, cljs.core.PersistentVector.EMPTY, fs__20212)
        };
        var G__20223__3 = function(x, y, z) {
          return cljs.core.reduce.call(null, function(p1__20199_SHARP_, p2__20200_SHARP_) {
            return cljs.core.conj.call(null, p1__20199_SHARP_, p2__20200_SHARP_.call(null, x, y, z))
          }, cljs.core.PersistentVector.EMPTY, fs__20212)
        };
        var G__20223__4 = function() {
          var G__20224__delegate = function(x, y, z, args) {
            return cljs.core.reduce.call(null, function(p1__20201_SHARP_, p2__20202_SHARP_) {
              return cljs.core.conj.call(null, p1__20201_SHARP_, cljs.core.apply.call(null, p2__20202_SHARP_, x, y, z, args))
            }, cljs.core.PersistentVector.EMPTY, fs__20212)
          };
          var G__20224 = function(x, y, z, var_args) {
            var args = null;
            if(goog.isDef(var_args)) {
              args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
            }
            return G__20224__delegate.call(this, x, y, z, args)
          };
          G__20224.cljs$lang$maxFixedArity = 3;
          G__20224.cljs$lang$applyTo = function(arglist__20225) {
            var x = cljs.core.first(arglist__20225);
            var y = cljs.core.first(cljs.core.next(arglist__20225));
            var z = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20225)));
            var args = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20225)));
            return G__20224__delegate(x, y, z, args)
          };
          G__20224.cljs$lang$arity$variadic = G__20224__delegate;
          return G__20224
        }();
        G__20223 = function(x, y, z, var_args) {
          var args = var_args;
          switch(arguments.length) {
            case 0:
              return G__20223__0.call(this);
            case 1:
              return G__20223__1.call(this, x);
            case 2:
              return G__20223__2.call(this, x, y);
            case 3:
              return G__20223__3.call(this, x, y, z);
            default:
              return G__20223__4.cljs$lang$arity$variadic(x, y, z, cljs.core.array_seq(arguments, 3))
          }
          throw"Invalid arity: " + arguments.length;
        };
        G__20223.cljs$lang$maxFixedArity = 3;
        G__20223.cljs$lang$applyTo = G__20223__4.cljs$lang$applyTo;
        return G__20223
      }()
    };
    var G__20222 = function(f, g, h, var_args) {
      var fs = null;
      if(goog.isDef(var_args)) {
        fs = cljs.core.array_seq(Array.prototype.slice.call(arguments, 3), 0)
      }
      return G__20222__delegate.call(this, f, g, h, fs)
    };
    G__20222.cljs$lang$maxFixedArity = 3;
    G__20222.cljs$lang$applyTo = function(arglist__20226) {
      var f = cljs.core.first(arglist__20226);
      var g = cljs.core.first(cljs.core.next(arglist__20226));
      var h = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20226)));
      var fs = cljs.core.rest(cljs.core.next(cljs.core.next(arglist__20226)));
      return G__20222__delegate(f, g, h, fs)
    };
    G__20222.cljs$lang$arity$variadic = G__20222__delegate;
    return G__20222
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
        var G__20229 = cljs.core.next.call(null, coll);
        coll = G__20229;
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
        var and__3822__auto____20228 = cljs.core.seq.call(null, coll);
        if(and__3822__auto____20228) {
          return n > 0
        }else {
          return and__3822__auto____20228
        }
      }())) {
        var G__20230 = n - 1;
        var G__20231 = cljs.core.next.call(null, coll);
        n = G__20230;
        coll = G__20231;
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
  var matches__20233 = re.exec(s);
  if(cljs.core._EQ_.call(null, cljs.core.first.call(null, matches__20233), s)) {
    if(cljs.core.count.call(null, matches__20233) === 1) {
      return cljs.core.first.call(null, matches__20233)
    }else {
      return cljs.core.vec.call(null, matches__20233)
    }
  }else {
    return null
  }
};
cljs.core.re_find = function re_find(re, s) {
  var matches__20235 = re.exec(s);
  if(matches__20235 == null) {
    return null
  }else {
    if(cljs.core.count.call(null, matches__20235) === 1) {
      return cljs.core.first.call(null, matches__20235)
    }else {
      return cljs.core.vec.call(null, matches__20235)
    }
  }
};
cljs.core.re_seq = function re_seq(re, s) {
  var match_data__20240 = cljs.core.re_find.call(null, re, s);
  var match_idx__20241 = s.search(re);
  var match_str__20242 = cljs.core.coll_QMARK_.call(null, match_data__20240) ? cljs.core.first.call(null, match_data__20240) : match_data__20240;
  var post_match__20243 = cljs.core.subs.call(null, s, match_idx__20241 + cljs.core.count.call(null, match_str__20242));
  if(cljs.core.truth_(match_data__20240)) {
    return new cljs.core.LazySeq(null, false, function() {
      return cljs.core.cons.call(null, match_data__20240, re_seq.call(null, re, post_match__20243))
    }, null)
  }else {
    return null
  }
};
cljs.core.re_pattern = function re_pattern(s) {
  var vec__20250__20251 = cljs.core.re_find.call(null, /^(?:\(\?([idmsux]*)\))?(.*)/, s);
  var ___20252 = cljs.core.nth.call(null, vec__20250__20251, 0, null);
  var flags__20253 = cljs.core.nth.call(null, vec__20250__20251, 1, null);
  var pattern__20254 = cljs.core.nth.call(null, vec__20250__20251, 2, null);
  return new RegExp(pattern__20254, flags__20253)
};
cljs.core.pr_sequential = function pr_sequential(print_one, begin, sep, end, opts, coll) {
  return cljs.core.concat.call(null, cljs.core.PersistentVector.fromArray([begin], true), cljs.core.flatten1.call(null, cljs.core.interpose.call(null, cljs.core.PersistentVector.fromArray([sep], true), cljs.core.map.call(null, function(p1__20244_SHARP_) {
    return print_one.call(null, p1__20244_SHARP_, opts)
  }, coll))), cljs.core.PersistentVector.fromArray([end], true))
};
cljs.core.pr_sequential_writer = function pr_sequential_writer(writer, print_one, begin, sep, end, opts, coll) {
  cljs.core._write.call(null, writer, begin);
  if(cljs.core.seq.call(null, coll)) {
    print_one.call(null, cljs.core.first.call(null, coll), writer, opts)
  }else {
  }
  var G__20258__20259 = cljs.core.seq.call(null, cljs.core.next.call(null, coll));
  while(true) {
    if(G__20258__20259) {
      var o__20260 = cljs.core.first.call(null, G__20258__20259);
      cljs.core._write.call(null, writer, sep);
      print_one.call(null, o__20260, writer, opts);
      var G__20261 = cljs.core.next.call(null, G__20258__20259);
      G__20258__20259 = G__20261;
      continue
    }else {
    }
    break
  }
  return cljs.core._write.call(null, writer, end)
};
cljs.core.write_all = function() {
  var write_all__delegate = function(writer, ss) {
    var G__20265__20266 = cljs.core.seq.call(null, ss);
    while(true) {
      if(G__20265__20266) {
        var s__20267 = cljs.core.first.call(null, G__20265__20266);
        cljs.core._write.call(null, writer, s__20267);
        var G__20268 = cljs.core.next.call(null, G__20265__20266);
        G__20265__20266 = G__20268;
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
  write_all.cljs$lang$applyTo = function(arglist__20269) {
    var writer = cljs.core.first(arglist__20269);
    var ss = cljs.core.rest(arglist__20269);
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
  var this__20270 = this;
  return this__20270.sb.append(s)
};
cljs.core.StringBufferWriter.prototype.cljs$core$IWriter$_flush$arity$1 = function(_) {
  var this__20271 = this;
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
          var and__3822__auto____20281 = cljs.core._lookup.call(null, opts, "\ufdd0'meta", null);
          if(cljs.core.truth_(and__3822__auto____20281)) {
            var and__3822__auto____20285 = function() {
              var G__20282__20283 = obj;
              if(G__20282__20283) {
                if(function() {
                  var or__3824__auto____20284 = G__20282__20283.cljs$lang$protocol_mask$partition0$ & 131072;
                  if(or__3824__auto____20284) {
                    return or__3824__auto____20284
                  }else {
                    return G__20282__20283.cljs$core$IMeta$
                  }
                }()) {
                  return true
                }else {
                  if(!G__20282__20283.cljs$lang$protocol_mask$partition0$) {
                    return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__20282__20283)
                  }else {
                    return false
                  }
                }
              }else {
                return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__20282__20283)
              }
            }();
            if(cljs.core.truth_(and__3822__auto____20285)) {
              return cljs.core.meta.call(null, obj)
            }else {
              return and__3822__auto____20285
            }
          }else {
            return and__3822__auto____20281
          }
        }()) ? cljs.core.concat.call(null, cljs.core.PersistentVector.fromArray(["^"], true), pr_seq.call(null, cljs.core.meta.call(null, obj), opts), cljs.core.PersistentVector.fromArray([" "], true)) : null, function() {
          var and__3822__auto____20286 = !(obj == null);
          if(and__3822__auto____20286) {
            return obj.cljs$lang$type
          }else {
            return and__3822__auto____20286
          }
        }() ? obj.cljs$lang$ctorPrSeq(obj) : function() {
          var G__20287__20288 = obj;
          if(G__20287__20288) {
            if(function() {
              var or__3824__auto____20289 = G__20287__20288.cljs$lang$protocol_mask$partition0$ & 536870912;
              if(or__3824__auto____20289) {
                return or__3824__auto____20289
              }else {
                return G__20287__20288.cljs$core$IPrintable$
              }
            }()) {
              return true
            }else {
              if(!G__20287__20288.cljs$lang$protocol_mask$partition0$) {
                return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__20287__20288)
              }else {
                return false
              }
            }
          }else {
            return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__20287__20288)
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
          var and__3822__auto____20302 = cljs.core._lookup.call(null, opts, "\ufdd0'meta", null);
          if(cljs.core.truth_(and__3822__auto____20302)) {
            var and__3822__auto____20306 = function() {
              var G__20303__20304 = obj;
              if(G__20303__20304) {
                if(function() {
                  var or__3824__auto____20305 = G__20303__20304.cljs$lang$protocol_mask$partition0$ & 131072;
                  if(or__3824__auto____20305) {
                    return or__3824__auto____20305
                  }else {
                    return G__20303__20304.cljs$core$IMeta$
                  }
                }()) {
                  return true
                }else {
                  if(!G__20303__20304.cljs$lang$protocol_mask$partition0$) {
                    return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__20303__20304)
                  }else {
                    return false
                  }
                }
              }else {
                return cljs.core.type_satisfies_.call(null, cljs.core.IMeta, G__20303__20304)
              }
            }();
            if(cljs.core.truth_(and__3822__auto____20306)) {
              return cljs.core.meta.call(null, obj)
            }else {
              return and__3822__auto____20306
            }
          }else {
            return and__3822__auto____20302
          }
        }())) {
          cljs.core._write.call(null, writer, "^");
          pr_writer.call(null, cljs.core.meta.call(null, obj), writer, opts);
          cljs.core._write.call(null, writer, " ")
        }else {
        }
        if(function() {
          var and__3822__auto____20307 = !(obj == null);
          if(and__3822__auto____20307) {
            return obj.cljs$lang$type
          }else {
            return and__3822__auto____20307
          }
        }()) {
          return obj.cljs$lang$ctorPrWriter(writer, opts)
        }else {
          if(function() {
            var G__20308__20309 = obj;
            if(G__20308__20309) {
              if(function() {
                var or__3824__auto____20310 = G__20308__20309.cljs$lang$protocol_mask$partition0$ & 2147483648;
                if(or__3824__auto____20310) {
                  return or__3824__auto____20310
                }else {
                  return G__20308__20309.cljs$core$IPrintWithWriter$
                }
              }()) {
                return true
              }else {
                if(!G__20308__20309.cljs$lang$protocol_mask$partition0$) {
                  return cljs.core.type_satisfies_.call(null, cljs.core.IPrintWithWriter, G__20308__20309)
                }else {
                  return false
                }
              }
            }else {
              return cljs.core.type_satisfies_.call(null, cljs.core.IPrintWithWriter, G__20308__20309)
            }
          }()) {
            return cljs.core._pr_writer.call(null, obj, writer, opts)
          }else {
            if(function() {
              var G__20311__20312 = obj;
              if(G__20311__20312) {
                if(function() {
                  var or__3824__auto____20313 = G__20311__20312.cljs$lang$protocol_mask$partition0$ & 536870912;
                  if(or__3824__auto____20313) {
                    return or__3824__auto____20313
                  }else {
                    return G__20311__20312.cljs$core$IPrintable$
                  }
                }()) {
                  return true
                }else {
                  if(!G__20311__20312.cljs$lang$protocol_mask$partition0$) {
                    return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__20311__20312)
                  }else {
                    return false
                  }
                }
              }else {
                return cljs.core.type_satisfies_.call(null, cljs.core.IPrintable, G__20311__20312)
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
  var G__20317__20318 = cljs.core.seq.call(null, cljs.core.next.call(null, objs));
  while(true) {
    if(G__20317__20318) {
      var obj__20319 = cljs.core.first.call(null, G__20317__20318);
      cljs.core._write.call(null, writer, " ");
      cljs.core.pr_writer.call(null, obj__20319, writer, opts);
      var G__20320 = cljs.core.next.call(null, G__20317__20318);
      G__20317__20318 = G__20320;
      continue
    }else {
      return null
    }
    break
  }
};
cljs.core.pr_sb_with_opts = function pr_sb_with_opts(objs, opts) {
  var sb__20323 = new goog.string.StringBuffer;
  var writer__20324 = new cljs.core.StringBufferWriter(sb__20323);
  cljs.core.pr_seq_writer.call(null, objs, writer__20324, opts);
  cljs.core._flush.call(null, writer__20324);
  return sb__20323
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
    var sb__20326 = cljs.core.pr_sb_with_opts.call(null, objs, opts);
    sb__20326.append("\n");
    return[cljs.core.str(sb__20326)].join("")
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
  pr_str.cljs$lang$applyTo = function(arglist__20327) {
    var objs = cljs.core.seq(arglist__20327);
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
  prn_str.cljs$lang$applyTo = function(arglist__20328) {
    var objs = cljs.core.seq(arglist__20328);
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
  pr.cljs$lang$applyTo = function(arglist__20329) {
    var objs = cljs.core.seq(arglist__20329);
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
  cljs_core_print.cljs$lang$applyTo = function(arglist__20330) {
    var objs = cljs.core.seq(arglist__20330);
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
  print_str.cljs$lang$applyTo = function(arglist__20331) {
    var objs = cljs.core.seq(arglist__20331);
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
  println.cljs$lang$applyTo = function(arglist__20332) {
    var objs = cljs.core.seq(arglist__20332);
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
  println_str.cljs$lang$applyTo = function(arglist__20333) {
    var objs = cljs.core.seq(arglist__20333);
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
  prn.cljs$lang$applyTo = function(arglist__20334) {
    var objs = cljs.core.seq(arglist__20334);
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
  printf.cljs$lang$applyTo = function(arglist__20335) {
    var fmt = cljs.core.first(arglist__20335);
    var args = cljs.core.rest(arglist__20335);
    return printf__delegate(fmt, args)
  };
  printf.cljs$lang$arity$variadic = printf__delegate;
  return printf
}();
cljs.core.HashMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.HashMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__20336 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__20336, "{", ", ", "}", opts, coll)
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
  var pr_pair__20337 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__20337, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  var pr_pair__20338 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__20338, "{", ", ", "}", opts, coll)
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
      var temp__3974__auto____20339 = cljs.core.namespace.call(null, obj);
      if(cljs.core.truth_(temp__3974__auto____20339)) {
        var nspc__20340 = temp__3974__auto____20339;
        return[cljs.core.str(nspc__20340), cljs.core.str("/")].join("")
      }else {
        return null
      }
    }()), cljs.core.str(cljs.core.name.call(null, obj))].join(""))
  }else {
    if(cljs.core.symbol_QMARK_.call(null, obj)) {
      return cljs.core.list.call(null, [cljs.core.str(function() {
        var temp__3974__auto____20341 = cljs.core.namespace.call(null, obj);
        if(cljs.core.truth_(temp__3974__auto____20341)) {
          var nspc__20342 = temp__3974__auto____20341;
          return[cljs.core.str(nspc__20342), cljs.core.str("/")].join("")
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
  var pr_pair__20343 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__20343, "{", ", ", "}", opts, coll)
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
  var normalize__20345 = function(n, len) {
    var ns__20344 = [cljs.core.str(n)].join("");
    while(true) {
      if(cljs.core.count.call(null, ns__20344) < len) {
        var G__20347 = [cljs.core.str("0"), cljs.core.str(ns__20344)].join("");
        ns__20344 = G__20347;
        continue
      }else {
        return ns__20344
      }
      break
    }
  };
  return cljs.core.list.call(null, [cljs.core.str('#inst "'), cljs.core.str(d.getUTCFullYear()), cljs.core.str("-"), cljs.core.str(normalize__20345.call(null, d.getUTCMonth() + 1, 2)), cljs.core.str("-"), cljs.core.str(normalize__20345.call(null, d.getUTCDate(), 2)), cljs.core.str("T"), cljs.core.str(normalize__20345.call(null, d.getUTCHours(), 2)), cljs.core.str(":"), cljs.core.str(normalize__20345.call(null, d.getUTCMinutes(), 2)), cljs.core.str(":"), cljs.core.str(normalize__20345.call(null, d.getUTCSeconds(), 
  2)), cljs.core.str("."), cljs.core.str(normalize__20345.call(null, d.getUTCMilliseconds(), 3)), cljs.core.str("-"), cljs.core.str('00:00"')].join(""))
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
  var pr_pair__20346 = function(keyval) {
    return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential.call(null, pr_pair__20346, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintable$ = true;
cljs.core.PersistentTreeMapSeq.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(coll, opts) {
  return cljs.core.pr_sequential.call(null, cljs.core.pr_seq, "(", " ", ")", opts, coll)
};
cljs.core.HashMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.HashMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__20348 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__20348, "{", ", ", "}", opts, coll)
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
  var pr_pair__20349 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__20349, "{", ", ", "}", opts, coll)
};
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintWithWriter$ = true;
cljs.core.PersistentArrayMap.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(coll, writer, opts) {
  var pr_pair__20350 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__20350, "{", ", ", "}", opts, coll)
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
    var temp__3974__auto____20351 = cljs.core.namespace.call(null, obj);
    if(cljs.core.truth_(temp__3974__auto____20351)) {
      var nspc__20352 = temp__3974__auto____20351;
      cljs.core.write_all.call(null, writer, [cljs.core.str(nspc__20352)].join(""), "/")
    }else {
    }
    return cljs.core._write.call(null, writer, cljs.core.name.call(null, obj))
  }else {
    if(cljs.core.symbol_QMARK_.call(null, obj)) {
      var temp__3974__auto____20353 = cljs.core.namespace.call(null, obj);
      if(cljs.core.truth_(temp__3974__auto____20353)) {
        var nspc__20354 = temp__3974__auto____20353;
        cljs.core.write_all.call(null, writer, [cljs.core.str(nspc__20354)].join(""), "/")
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
  var pr_pair__20355 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__20355, "{", ", ", "}", opts, coll)
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
  var normalize__20357 = function(n, len) {
    var ns__20356 = [cljs.core.str(n)].join("");
    while(true) {
      if(cljs.core.count.call(null, ns__20356) < len) {
        var G__20359 = [cljs.core.str("0"), cljs.core.str(ns__20356)].join("");
        ns__20356 = G__20359;
        continue
      }else {
        return ns__20356
      }
      break
    }
  };
  return cljs.core.write_all.call(null, writer, '#inst "', [cljs.core.str(d.getUTCFullYear())].join(""), "-", normalize__20357.call(null, d.getUTCMonth() + 1, 2), "-", normalize__20357.call(null, d.getUTCDate(), 2), "T", normalize__20357.call(null, d.getUTCHours(), 2), ":", normalize__20357.call(null, d.getUTCMinutes(), 2), ":", normalize__20357.call(null, d.getUTCSeconds(), 2), ".", normalize__20357.call(null, d.getUTCMilliseconds(), 3), "-", '00:00"')
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
  var pr_pair__20358 = function(keyval) {
    return cljs.core.pr_sequential_writer.call(null, writer, cljs.core.pr_writer, "", " ", "", opts, keyval)
  };
  return cljs.core.pr_sequential_writer.call(null, writer, pr_pair__20358, "{", ", ", "}", opts, coll)
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
  var this__20360 = this;
  return goog.getUid(this$)
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_notify_watches$arity$3 = function(this$, oldval, newval) {
  var this__20361 = this;
  var G__20362__20363 = cljs.core.seq.call(null, this__20361.watches);
  while(true) {
    if(G__20362__20363) {
      var vec__20364__20365 = cljs.core.first.call(null, G__20362__20363);
      var key__20366 = cljs.core.nth.call(null, vec__20364__20365, 0, null);
      var f__20367 = cljs.core.nth.call(null, vec__20364__20365, 1, null);
      f__20367.call(null, key__20366, this$, oldval, newval);
      var G__20375 = cljs.core.next.call(null, G__20362__20363);
      G__20362__20363 = G__20375;
      continue
    }else {
      return null
    }
    break
  }
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_add_watch$arity$3 = function(this$, key, f) {
  var this__20368 = this;
  return this$.watches = cljs.core.assoc.call(null, this__20368.watches, key, f)
};
cljs.core.Atom.prototype.cljs$core$IWatchable$_remove_watch$arity$2 = function(this$, key) {
  var this__20369 = this;
  return this$.watches = cljs.core.dissoc.call(null, this__20369.watches, key)
};
cljs.core.Atom.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(a, writer, opts) {
  var this__20370 = this;
  cljs.core._write.call(null, writer, "#<Atom: ");
  cljs.core._pr_writer.call(null, this__20370.state, writer, opts);
  return cljs.core._write.call(null, writer, ">")
};
cljs.core.Atom.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(a, opts) {
  var this__20371 = this;
  return cljs.core.concat.call(null, cljs.core.PersistentVector.fromArray(["#<Atom: "], true), cljs.core._pr_seq.call(null, this__20371.state, opts), ">")
};
cljs.core.Atom.prototype.cljs$core$IMeta$_meta$arity$1 = function(_) {
  var this__20372 = this;
  return this__20372.meta
};
cljs.core.Atom.prototype.cljs$core$IDeref$_deref$arity$1 = function(_) {
  var this__20373 = this;
  return this__20373.state
};
cljs.core.Atom.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(o, other) {
  var this__20374 = this;
  return o === other
};
cljs.core.Atom;
cljs.core.atom = function() {
  var atom = null;
  var atom__1 = function(x) {
    return new cljs.core.Atom(x, null, null, null)
  };
  var atom__2 = function() {
    var G__20387__delegate = function(x, p__20376) {
      var map__20382__20383 = p__20376;
      var map__20382__20384 = cljs.core.seq_QMARK_.call(null, map__20382__20383) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20382__20383) : map__20382__20383;
      var validator__20385 = cljs.core._lookup.call(null, map__20382__20384, "\ufdd0'validator", null);
      var meta__20386 = cljs.core._lookup.call(null, map__20382__20384, "\ufdd0'meta", null);
      return new cljs.core.Atom(x, meta__20386, validator__20385, null)
    };
    var G__20387 = function(x, var_args) {
      var p__20376 = null;
      if(goog.isDef(var_args)) {
        p__20376 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__20387__delegate.call(this, x, p__20376)
    };
    G__20387.cljs$lang$maxFixedArity = 1;
    G__20387.cljs$lang$applyTo = function(arglist__20388) {
      var x = cljs.core.first(arglist__20388);
      var p__20376 = cljs.core.rest(arglist__20388);
      return G__20387__delegate(x, p__20376)
    };
    G__20387.cljs$lang$arity$variadic = G__20387__delegate;
    return G__20387
  }();
  atom = function(x, var_args) {
    var p__20376 = var_args;
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
  var temp__3974__auto____20392 = a.validator;
  if(cljs.core.truth_(temp__3974__auto____20392)) {
    var validate__20393 = temp__3974__auto____20392;
    if(cljs.core.truth_(validate__20393.call(null, new_value))) {
    }else {
      throw new Error([cljs.core.str("Assert failed: "), cljs.core.str("Validator rejected reference state"), cljs.core.str("\n"), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'validate", "\ufdd1'new-value"), cljs.core.hash_map("\ufdd0'line", 6683))))].join(""));
    }
  }else {
  }
  var old_value__20394 = a.state;
  a.state = new_value;
  cljs.core._notify_watches.call(null, a, old_value__20394, new_value);
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
    var G__20395__delegate = function(a, f, x, y, z, more) {
      return cljs.core.reset_BANG_.call(null, a, cljs.core.apply.call(null, f, a.state, x, y, z, more))
    };
    var G__20395 = function(a, f, x, y, z, var_args) {
      var more = null;
      if(goog.isDef(var_args)) {
        more = cljs.core.array_seq(Array.prototype.slice.call(arguments, 5), 0)
      }
      return G__20395__delegate.call(this, a, f, x, y, z, more)
    };
    G__20395.cljs$lang$maxFixedArity = 5;
    G__20395.cljs$lang$applyTo = function(arglist__20396) {
      var a = cljs.core.first(arglist__20396);
      var f = cljs.core.first(cljs.core.next(arglist__20396));
      var x = cljs.core.first(cljs.core.next(cljs.core.next(arglist__20396)));
      var y = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(arglist__20396))));
      var z = cljs.core.first(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__20396)))));
      var more = cljs.core.rest(cljs.core.next(cljs.core.next(cljs.core.next(cljs.core.next(arglist__20396)))));
      return G__20395__delegate(a, f, x, y, z, more)
    };
    G__20395.cljs$lang$arity$variadic = G__20395__delegate;
    return G__20395
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
  alter_meta_BANG_.cljs$lang$applyTo = function(arglist__20397) {
    var iref = cljs.core.first(arglist__20397);
    var f = cljs.core.first(cljs.core.next(arglist__20397));
    var args = cljs.core.rest(cljs.core.next(arglist__20397));
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
  var this__20398 = this;
  return(new cljs.core.Keyword("\ufdd0'done")).call(null, cljs.core.deref.call(null, this__20398.state))
};
cljs.core.Delay.prototype.cljs$core$IDeref$_deref$arity$1 = function(_) {
  var this__20399 = this;
  return(new cljs.core.Keyword("\ufdd0'value")).call(null, cljs.core.swap_BANG_.call(null, this__20399.state, function(p__20400) {
    var map__20401__20402 = p__20400;
    var map__20401__20403 = cljs.core.seq_QMARK_.call(null, map__20401__20402) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20401__20402) : map__20401__20402;
    var curr_state__20404 = map__20401__20403;
    var done__20405 = cljs.core._lookup.call(null, map__20401__20403, "\ufdd0'done", null);
    if(cljs.core.truth_(done__20405)) {
      return curr_state__20404
    }else {
      return cljs.core.ObjMap.fromObject(["\ufdd0'done", "\ufdd0'value"], {"\ufdd0'done":true, "\ufdd0'value":this__20399.f.call(null)})
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
    var map__20426__20427 = options;
    var map__20426__20428 = cljs.core.seq_QMARK_.call(null, map__20426__20427) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20426__20427) : map__20426__20427;
    var keywordize_keys__20429 = cljs.core._lookup.call(null, map__20426__20428, "\ufdd0'keywordize-keys", null);
    var keyfn__20430 = cljs.core.truth_(keywordize_keys__20429) ? cljs.core.keyword : cljs.core.str;
    var f__20445 = function thisfn(x) {
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
                var iter__2528__auto____20444 = function iter__20438(s__20439) {
                  return new cljs.core.LazySeq(null, false, function() {
                    var s__20439__20442 = s__20439;
                    while(true) {
                      if(cljs.core.seq.call(null, s__20439__20442)) {
                        var k__20443 = cljs.core.first.call(null, s__20439__20442);
                        return cljs.core.cons.call(null, cljs.core.PersistentVector.fromArray([keyfn__20430.call(null, k__20443), thisfn.call(null, x[k__20443])], true), iter__20438.call(null, cljs.core.rest.call(null, s__20439__20442)))
                      }else {
                        return null
                      }
                      break
                    }
                  }, null)
                };
                return iter__2528__auto____20444.call(null, cljs.core.js_keys.call(null, x))
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
    return f__20445.call(null, x)
  };
  var js__GT_clj = function(x, var_args) {
    var options = null;
    if(goog.isDef(var_args)) {
      options = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return js__GT_clj__delegate.call(this, x, options)
  };
  js__GT_clj.cljs$lang$maxFixedArity = 1;
  js__GT_clj.cljs$lang$applyTo = function(arglist__20446) {
    var x = cljs.core.first(arglist__20446);
    var options = cljs.core.rest(arglist__20446);
    return js__GT_clj__delegate(x, options)
  };
  js__GT_clj.cljs$lang$arity$variadic = js__GT_clj__delegate;
  return js__GT_clj
}();
cljs.core.memoize = function memoize(f) {
  var mem__20451 = cljs.core.atom.call(null, cljs.core.ObjMap.EMPTY);
  return function() {
    var G__20455__delegate = function(args) {
      var temp__3971__auto____20452 = cljs.core._lookup.call(null, cljs.core.deref.call(null, mem__20451), args, null);
      if(cljs.core.truth_(temp__3971__auto____20452)) {
        var v__20453 = temp__3971__auto____20452;
        return v__20453
      }else {
        var ret__20454 = cljs.core.apply.call(null, f, args);
        cljs.core.swap_BANG_.call(null, mem__20451, cljs.core.assoc, args, ret__20454);
        return ret__20454
      }
    };
    var G__20455 = function(var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__20455__delegate.call(this, args)
    };
    G__20455.cljs$lang$maxFixedArity = 0;
    G__20455.cljs$lang$applyTo = function(arglist__20456) {
      var args = cljs.core.seq(arglist__20456);
      return G__20455__delegate(args)
    };
    G__20455.cljs$lang$arity$variadic = G__20455__delegate;
    return G__20455
  }()
};
cljs.core.trampoline = function() {
  var trampoline = null;
  var trampoline__1 = function(f) {
    while(true) {
      var ret__20458 = f.call(null);
      if(cljs.core.fn_QMARK_.call(null, ret__20458)) {
        var G__20459 = ret__20458;
        f = G__20459;
        continue
      }else {
        return ret__20458
      }
      break
    }
  };
  var trampoline__2 = function() {
    var G__20460__delegate = function(f, args) {
      return trampoline.call(null, function() {
        return cljs.core.apply.call(null, f, args)
      })
    };
    var G__20460 = function(f, var_args) {
      var args = null;
      if(goog.isDef(var_args)) {
        args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
      }
      return G__20460__delegate.call(this, f, args)
    };
    G__20460.cljs$lang$maxFixedArity = 1;
    G__20460.cljs$lang$applyTo = function(arglist__20461) {
      var f = cljs.core.first(arglist__20461);
      var args = cljs.core.rest(arglist__20461);
      return G__20460__delegate(f, args)
    };
    G__20460.cljs$lang$arity$variadic = G__20460__delegate;
    return G__20460
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
    var k__20463 = f.call(null, x);
    return cljs.core.assoc.call(null, ret, k__20463, cljs.core.conj.call(null, cljs.core._lookup.call(null, ret, k__20463, cljs.core.PersistentVector.EMPTY), x))
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
    var or__3824__auto____20472 = cljs.core._EQ_.call(null, child, parent);
    if(or__3824__auto____20472) {
      return or__3824__auto____20472
    }else {
      var or__3824__auto____20473 = cljs.core.contains_QMARK_.call(null, (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h).call(null, child), parent);
      if(or__3824__auto____20473) {
        return or__3824__auto____20473
      }else {
        var and__3822__auto____20474 = cljs.core.vector_QMARK_.call(null, parent);
        if(and__3822__auto____20474) {
          var and__3822__auto____20475 = cljs.core.vector_QMARK_.call(null, child);
          if(and__3822__auto____20475) {
            var and__3822__auto____20476 = cljs.core.count.call(null, parent) === cljs.core.count.call(null, child);
            if(and__3822__auto____20476) {
              var ret__20477 = true;
              var i__20478 = 0;
              while(true) {
                if(function() {
                  var or__3824__auto____20479 = cljs.core.not.call(null, ret__20477);
                  if(or__3824__auto____20479) {
                    return or__3824__auto____20479
                  }else {
                    return i__20478 === cljs.core.count.call(null, parent)
                  }
                }()) {
                  return ret__20477
                }else {
                  var G__20480 = isa_QMARK_.call(null, h, child.call(null, i__20478), parent.call(null, i__20478));
                  var G__20481 = i__20478 + 1;
                  ret__20477 = G__20480;
                  i__20478 = G__20481;
                  continue
                }
                break
              }
            }else {
              return and__3822__auto____20476
            }
          }else {
            return and__3822__auto____20475
          }
        }else {
          return and__3822__auto____20474
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
    var tp__20490 = (new cljs.core.Keyword("\ufdd0'parents")).call(null, h);
    var td__20491 = (new cljs.core.Keyword("\ufdd0'descendants")).call(null, h);
    var ta__20492 = (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h);
    var tf__20493 = function(m, source, sources, target, targets) {
      return cljs.core.reduce.call(null, function(ret, k) {
        return cljs.core.assoc.call(null, ret, k, cljs.core.reduce.call(null, cljs.core.conj, cljs.core._lookup.call(null, targets, k, cljs.core.PersistentHashSet.EMPTY), cljs.core.cons.call(null, target, targets.call(null, target))))
      }, m, cljs.core.cons.call(null, source, sources.call(null, source)))
    };
    var or__3824__auto____20494 = cljs.core.contains_QMARK_.call(null, tp__20490.call(null, tag), parent) ? null : function() {
      if(cljs.core.contains_QMARK_.call(null, ta__20492.call(null, tag), parent)) {
        throw new Error([cljs.core.str(tag), cljs.core.str("already has"), cljs.core.str(parent), cljs.core.str("as ancestor")].join(""));
      }else {
      }
      if(cljs.core.contains_QMARK_.call(null, ta__20492.call(null, parent), tag)) {
        throw new Error([cljs.core.str("Cyclic derivation:"), cljs.core.str(parent), cljs.core.str("has"), cljs.core.str(tag), cljs.core.str("as ancestor")].join(""));
      }else {
      }
      return cljs.core.ObjMap.fromObject(["\ufdd0'parents", "\ufdd0'ancestors", "\ufdd0'descendants"], {"\ufdd0'parents":cljs.core.assoc.call(null, (new cljs.core.Keyword("\ufdd0'parents")).call(null, h), tag, cljs.core.conj.call(null, cljs.core._lookup.call(null, tp__20490, tag, cljs.core.PersistentHashSet.EMPTY), parent)), "\ufdd0'ancestors":tf__20493.call(null, (new cljs.core.Keyword("\ufdd0'ancestors")).call(null, h), tag, td__20491, parent, ta__20492), "\ufdd0'descendants":tf__20493.call(null, 
      (new cljs.core.Keyword("\ufdd0'descendants")).call(null, h), parent, ta__20492, tag, td__20491)})
    }();
    if(cljs.core.truth_(or__3824__auto____20494)) {
      return or__3824__auto____20494
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
    var parentMap__20499 = (new cljs.core.Keyword("\ufdd0'parents")).call(null, h);
    var childsParents__20500 = cljs.core.truth_(parentMap__20499.call(null, tag)) ? cljs.core.disj.call(null, parentMap__20499.call(null, tag), parent) : cljs.core.PersistentHashSet.EMPTY;
    var newParents__20501 = cljs.core.truth_(cljs.core.not_empty.call(null, childsParents__20500)) ? cljs.core.assoc.call(null, parentMap__20499, tag, childsParents__20500) : cljs.core.dissoc.call(null, parentMap__20499, tag);
    var deriv_seq__20502 = cljs.core.flatten.call(null, cljs.core.map.call(null, function(p1__20482_SHARP_) {
      return cljs.core.cons.call(null, cljs.core.first.call(null, p1__20482_SHARP_), cljs.core.interpose.call(null, cljs.core.first.call(null, p1__20482_SHARP_), cljs.core.second.call(null, p1__20482_SHARP_)))
    }, cljs.core.seq.call(null, newParents__20501)));
    if(cljs.core.contains_QMARK_.call(null, parentMap__20499.call(null, tag), parent)) {
      return cljs.core.reduce.call(null, function(p1__20483_SHARP_, p2__20484_SHARP_) {
        return cljs.core.apply.call(null, cljs.core.derive, p1__20483_SHARP_, p2__20484_SHARP_)
      }, cljs.core.make_hierarchy.call(null), cljs.core.partition.call(null, 2, deriv_seq__20502))
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
  var xprefs__20510 = cljs.core.deref.call(null, prefer_table).call(null, x);
  var or__3824__auto____20512 = cljs.core.truth_(function() {
    var and__3822__auto____20511 = xprefs__20510;
    if(cljs.core.truth_(and__3822__auto____20511)) {
      return xprefs__20510.call(null, y)
    }else {
      return and__3822__auto____20511
    }
  }()) ? true : null;
  if(cljs.core.truth_(or__3824__auto____20512)) {
    return or__3824__auto____20512
  }else {
    var or__3824__auto____20514 = function() {
      var ps__20513 = cljs.core.parents.call(null, y);
      while(true) {
        if(cljs.core.count.call(null, ps__20513) > 0) {
          if(cljs.core.truth_(prefers_STAR_.call(null, x, cljs.core.first.call(null, ps__20513), prefer_table))) {
          }else {
          }
          var G__20517 = cljs.core.rest.call(null, ps__20513);
          ps__20513 = G__20517;
          continue
        }else {
          return null
        }
        break
      }
    }();
    if(cljs.core.truth_(or__3824__auto____20514)) {
      return or__3824__auto____20514
    }else {
      var or__3824__auto____20516 = function() {
        var ps__20515 = cljs.core.parents.call(null, x);
        while(true) {
          if(cljs.core.count.call(null, ps__20515) > 0) {
            if(cljs.core.truth_(prefers_STAR_.call(null, cljs.core.first.call(null, ps__20515), y, prefer_table))) {
            }else {
            }
            var G__20518 = cljs.core.rest.call(null, ps__20515);
            ps__20515 = G__20518;
            continue
          }else {
            return null
          }
          break
        }
      }();
      if(cljs.core.truth_(or__3824__auto____20516)) {
        return or__3824__auto____20516
      }else {
        return false
      }
    }
  }
};
cljs.core.dominates = function dominates(x, y, prefer_table) {
  var or__3824__auto____20520 = cljs.core.prefers_STAR_.call(null, x, y, prefer_table);
  if(cljs.core.truth_(or__3824__auto____20520)) {
    return or__3824__auto____20520
  }else {
    return cljs.core.isa_QMARK_.call(null, x, y)
  }
};
cljs.core.find_and_cache_best_method = function find_and_cache_best_method(name, dispatch_val, hierarchy, method_table, prefer_table, method_cache, cached_hierarchy) {
  var best_entry__20538 = cljs.core.reduce.call(null, function(be, p__20530) {
    var vec__20531__20532 = p__20530;
    var k__20533 = cljs.core.nth.call(null, vec__20531__20532, 0, null);
    var ___20534 = cljs.core.nth.call(null, vec__20531__20532, 1, null);
    var e__20535 = vec__20531__20532;
    if(cljs.core.isa_QMARK_.call(null, dispatch_val, k__20533)) {
      var be2__20537 = cljs.core.truth_(function() {
        var or__3824__auto____20536 = be == null;
        if(or__3824__auto____20536) {
          return or__3824__auto____20536
        }else {
          return cljs.core.dominates.call(null, k__20533, cljs.core.first.call(null, be), prefer_table)
        }
      }()) ? e__20535 : be;
      if(cljs.core.truth_(cljs.core.dominates.call(null, cljs.core.first.call(null, be2__20537), k__20533, prefer_table))) {
      }else {
        throw new Error([cljs.core.str("Multiple methods in multimethod '"), cljs.core.str(name), cljs.core.str("' match dispatch value: "), cljs.core.str(dispatch_val), cljs.core.str(" -> "), cljs.core.str(k__20533), cljs.core.str(" and "), cljs.core.str(cljs.core.first.call(null, be2__20537)), cljs.core.str(", and neither is preferred")].join(""));
      }
      return be2__20537
    }else {
      return be
    }
  }, null, cljs.core.deref.call(null, method_table));
  if(cljs.core.truth_(best_entry__20538)) {
    if(cljs.core._EQ_.call(null, cljs.core.deref.call(null, cached_hierarchy), cljs.core.deref.call(null, hierarchy))) {
      cljs.core.swap_BANG_.call(null, method_cache, cljs.core.assoc, dispatch_val, cljs.core.second.call(null, best_entry__20538));
      return cljs.core.second.call(null, best_entry__20538)
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
    var and__3822__auto____20543 = mf;
    if(and__3822__auto____20543) {
      return mf.cljs$core$IMultiFn$_reset$arity$1
    }else {
      return and__3822__auto____20543
    }
  }()) {
    return mf.cljs$core$IMultiFn$_reset$arity$1(mf)
  }else {
    var x__2431__auto____20544 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20545 = cljs.core._reset[goog.typeOf(x__2431__auto____20544)];
      if(or__3824__auto____20545) {
        return or__3824__auto____20545
      }else {
        var or__3824__auto____20546 = cljs.core._reset["_"];
        if(or__3824__auto____20546) {
          return or__3824__auto____20546
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-reset", mf);
        }
      }
    }().call(null, mf)
  }
};
cljs.core._add_method = function _add_method(mf, dispatch_val, method) {
  if(function() {
    var and__3822__auto____20551 = mf;
    if(and__3822__auto____20551) {
      return mf.cljs$core$IMultiFn$_add_method$arity$3
    }else {
      return and__3822__auto____20551
    }
  }()) {
    return mf.cljs$core$IMultiFn$_add_method$arity$3(mf, dispatch_val, method)
  }else {
    var x__2431__auto____20552 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20553 = cljs.core._add_method[goog.typeOf(x__2431__auto____20552)];
      if(or__3824__auto____20553) {
        return or__3824__auto____20553
      }else {
        var or__3824__auto____20554 = cljs.core._add_method["_"];
        if(or__3824__auto____20554) {
          return or__3824__auto____20554
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-add-method", mf);
        }
      }
    }().call(null, mf, dispatch_val, method)
  }
};
cljs.core._remove_method = function _remove_method(mf, dispatch_val) {
  if(function() {
    var and__3822__auto____20559 = mf;
    if(and__3822__auto____20559) {
      return mf.cljs$core$IMultiFn$_remove_method$arity$2
    }else {
      return and__3822__auto____20559
    }
  }()) {
    return mf.cljs$core$IMultiFn$_remove_method$arity$2(mf, dispatch_val)
  }else {
    var x__2431__auto____20560 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20561 = cljs.core._remove_method[goog.typeOf(x__2431__auto____20560)];
      if(or__3824__auto____20561) {
        return or__3824__auto____20561
      }else {
        var or__3824__auto____20562 = cljs.core._remove_method["_"];
        if(or__3824__auto____20562) {
          return or__3824__auto____20562
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-remove-method", mf);
        }
      }
    }().call(null, mf, dispatch_val)
  }
};
cljs.core._prefer_method = function _prefer_method(mf, dispatch_val, dispatch_val_y) {
  if(function() {
    var and__3822__auto____20567 = mf;
    if(and__3822__auto____20567) {
      return mf.cljs$core$IMultiFn$_prefer_method$arity$3
    }else {
      return and__3822__auto____20567
    }
  }()) {
    return mf.cljs$core$IMultiFn$_prefer_method$arity$3(mf, dispatch_val, dispatch_val_y)
  }else {
    var x__2431__auto____20568 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20569 = cljs.core._prefer_method[goog.typeOf(x__2431__auto____20568)];
      if(or__3824__auto____20569) {
        return or__3824__auto____20569
      }else {
        var or__3824__auto____20570 = cljs.core._prefer_method["_"];
        if(or__3824__auto____20570) {
          return or__3824__auto____20570
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-prefer-method", mf);
        }
      }
    }().call(null, mf, dispatch_val, dispatch_val_y)
  }
};
cljs.core._get_method = function _get_method(mf, dispatch_val) {
  if(function() {
    var and__3822__auto____20575 = mf;
    if(and__3822__auto____20575) {
      return mf.cljs$core$IMultiFn$_get_method$arity$2
    }else {
      return and__3822__auto____20575
    }
  }()) {
    return mf.cljs$core$IMultiFn$_get_method$arity$2(mf, dispatch_val)
  }else {
    var x__2431__auto____20576 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20577 = cljs.core._get_method[goog.typeOf(x__2431__auto____20576)];
      if(or__3824__auto____20577) {
        return or__3824__auto____20577
      }else {
        var or__3824__auto____20578 = cljs.core._get_method["_"];
        if(or__3824__auto____20578) {
          return or__3824__auto____20578
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-get-method", mf);
        }
      }
    }().call(null, mf, dispatch_val)
  }
};
cljs.core._methods = function _methods(mf) {
  if(function() {
    var and__3822__auto____20583 = mf;
    if(and__3822__auto____20583) {
      return mf.cljs$core$IMultiFn$_methods$arity$1
    }else {
      return and__3822__auto____20583
    }
  }()) {
    return mf.cljs$core$IMultiFn$_methods$arity$1(mf)
  }else {
    var x__2431__auto____20584 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20585 = cljs.core._methods[goog.typeOf(x__2431__auto____20584)];
      if(or__3824__auto____20585) {
        return or__3824__auto____20585
      }else {
        var or__3824__auto____20586 = cljs.core._methods["_"];
        if(or__3824__auto____20586) {
          return or__3824__auto____20586
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-methods", mf);
        }
      }
    }().call(null, mf)
  }
};
cljs.core._prefers = function _prefers(mf) {
  if(function() {
    var and__3822__auto____20591 = mf;
    if(and__3822__auto____20591) {
      return mf.cljs$core$IMultiFn$_prefers$arity$1
    }else {
      return and__3822__auto____20591
    }
  }()) {
    return mf.cljs$core$IMultiFn$_prefers$arity$1(mf)
  }else {
    var x__2431__auto____20592 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20593 = cljs.core._prefers[goog.typeOf(x__2431__auto____20592)];
      if(or__3824__auto____20593) {
        return or__3824__auto____20593
      }else {
        var or__3824__auto____20594 = cljs.core._prefers["_"];
        if(or__3824__auto____20594) {
          return or__3824__auto____20594
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-prefers", mf);
        }
      }
    }().call(null, mf)
  }
};
cljs.core._dispatch = function _dispatch(mf, args) {
  if(function() {
    var and__3822__auto____20599 = mf;
    if(and__3822__auto____20599) {
      return mf.cljs$core$IMultiFn$_dispatch$arity$2
    }else {
      return and__3822__auto____20599
    }
  }()) {
    return mf.cljs$core$IMultiFn$_dispatch$arity$2(mf, args)
  }else {
    var x__2431__auto____20600 = mf == null ? null : mf;
    return function() {
      var or__3824__auto____20601 = cljs.core._dispatch[goog.typeOf(x__2431__auto____20600)];
      if(or__3824__auto____20601) {
        return or__3824__auto____20601
      }else {
        var or__3824__auto____20602 = cljs.core._dispatch["_"];
        if(or__3824__auto____20602) {
          return or__3824__auto____20602
        }else {
          throw cljs.core.missing_protocol.call(null, "IMultiFn.-dispatch", mf);
        }
      }
    }().call(null, mf, args)
  }
};
cljs.core.do_dispatch = function do_dispatch(mf, dispatch_fn, args) {
  var dispatch_val__20605 = cljs.core.apply.call(null, dispatch_fn, args);
  var target_fn__20606 = cljs.core._get_method.call(null, mf, dispatch_val__20605);
  if(cljs.core.truth_(target_fn__20606)) {
  }else {
    throw new Error([cljs.core.str("No method in multimethod '"), cljs.core.str(cljs.core.name), cljs.core.str("' for dispatch value: "), cljs.core.str(dispatch_val__20605)].join(""));
  }
  return cljs.core.apply.call(null, target_fn__20606, args)
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
  var this__20607 = this;
  return goog.getUid(this$)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_reset$arity$1 = function(mf) {
  var this__20608 = this;
  cljs.core.swap_BANG_.call(null, this__20608.method_table, function(mf) {
    return cljs.core.ObjMap.EMPTY
  });
  cljs.core.swap_BANG_.call(null, this__20608.method_cache, function(mf) {
    return cljs.core.ObjMap.EMPTY
  });
  cljs.core.swap_BANG_.call(null, this__20608.prefer_table, function(mf) {
    return cljs.core.ObjMap.EMPTY
  });
  cljs.core.swap_BANG_.call(null, this__20608.cached_hierarchy, function(mf) {
    return null
  });
  return mf
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_add_method$arity$3 = function(mf, dispatch_val, method) {
  var this__20609 = this;
  cljs.core.swap_BANG_.call(null, this__20609.method_table, cljs.core.assoc, dispatch_val, method);
  cljs.core.reset_cache.call(null, this__20609.method_cache, this__20609.method_table, this__20609.cached_hierarchy, this__20609.hierarchy);
  return mf
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_remove_method$arity$2 = function(mf, dispatch_val) {
  var this__20610 = this;
  cljs.core.swap_BANG_.call(null, this__20610.method_table, cljs.core.dissoc, dispatch_val);
  cljs.core.reset_cache.call(null, this__20610.method_cache, this__20610.method_table, this__20610.cached_hierarchy, this__20610.hierarchy);
  return mf
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_get_method$arity$2 = function(mf, dispatch_val) {
  var this__20611 = this;
  if(cljs.core._EQ_.call(null, cljs.core.deref.call(null, this__20611.cached_hierarchy), cljs.core.deref.call(null, this__20611.hierarchy))) {
  }else {
    cljs.core.reset_cache.call(null, this__20611.method_cache, this__20611.method_table, this__20611.cached_hierarchy, this__20611.hierarchy)
  }
  var temp__3971__auto____20612 = cljs.core.deref.call(null, this__20611.method_cache).call(null, dispatch_val);
  if(cljs.core.truth_(temp__3971__auto____20612)) {
    var target_fn__20613 = temp__3971__auto____20612;
    return target_fn__20613
  }else {
    var temp__3971__auto____20614 = cljs.core.find_and_cache_best_method.call(null, this__20611.name, dispatch_val, this__20611.hierarchy, this__20611.method_table, this__20611.prefer_table, this__20611.method_cache, this__20611.cached_hierarchy);
    if(cljs.core.truth_(temp__3971__auto____20614)) {
      var target_fn__20615 = temp__3971__auto____20614;
      return target_fn__20615
    }else {
      return cljs.core.deref.call(null, this__20611.method_table).call(null, this__20611.default_dispatch_val)
    }
  }
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_prefer_method$arity$3 = function(mf, dispatch_val_x, dispatch_val_y) {
  var this__20616 = this;
  if(cljs.core.truth_(cljs.core.prefers_STAR_.call(null, dispatch_val_x, dispatch_val_y, this__20616.prefer_table))) {
    throw new Error([cljs.core.str("Preference conflict in multimethod '"), cljs.core.str(this__20616.name), cljs.core.str("': "), cljs.core.str(dispatch_val_y), cljs.core.str(" is already preferred to "), cljs.core.str(dispatch_val_x)].join(""));
  }else {
  }
  cljs.core.swap_BANG_.call(null, this__20616.prefer_table, function(old) {
    return cljs.core.assoc.call(null, old, dispatch_val_x, cljs.core.conj.call(null, cljs.core._lookup.call(null, old, dispatch_val_x, cljs.core.PersistentHashSet.EMPTY), dispatch_val_y))
  });
  return cljs.core.reset_cache.call(null, this__20616.method_cache, this__20616.method_table, this__20616.cached_hierarchy, this__20616.hierarchy)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_methods$arity$1 = function(mf) {
  var this__20617 = this;
  return cljs.core.deref.call(null, this__20617.method_table)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_prefers$arity$1 = function(mf) {
  var this__20618 = this;
  return cljs.core.deref.call(null, this__20618.prefer_table)
};
cljs.core.MultiFn.prototype.cljs$core$IMultiFn$_dispatch$arity$2 = function(mf, args) {
  var this__20619 = this;
  return cljs.core.do_dispatch.call(null, mf, this__20619.dispatch_fn, args)
};
cljs.core.MultiFn;
cljs.core.MultiFn.prototype.call = function() {
  var G__20621__delegate = function(_, args) {
    var self__20620 = this;
    return cljs.core._dispatch.call(null, self__20620, args)
  };
  var G__20621 = function(_, var_args) {
    var args = null;
    if(goog.isDef(var_args)) {
      args = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return G__20621__delegate.call(this, _, args)
  };
  G__20621.cljs$lang$maxFixedArity = 1;
  G__20621.cljs$lang$applyTo = function(arglist__20622) {
    var _ = cljs.core.first(arglist__20622);
    var args = cljs.core.rest(arglist__20622);
    return G__20621__delegate(_, args)
  };
  G__20621.cljs$lang$arity$variadic = G__20621__delegate;
  return G__20621
}();
cljs.core.MultiFn.prototype.apply = function(_, args) {
  var self__20623 = this;
  return cljs.core._dispatch.call(null, self__20623, args)
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
  var this__20624 = this;
  return goog.string.hashCode(cljs.core.pr_str.call(null, this$))
};
cljs.core.UUID.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = function(_20626, writer, _) {
  var this__20625 = this;
  return cljs.core._write.call(null, writer, [cljs.core.str('#uuid "'), cljs.core.str(this__20625.uuid), cljs.core.str('"')].join(""))
};
cljs.core.UUID.prototype.cljs$core$IPrintable$_pr_seq$arity$2 = function(_20628, _) {
  var this__20627 = this;
  return cljs.core.list.call(null, [cljs.core.str('#uuid "'), cljs.core.str(this__20627.uuid), cljs.core.str('"')].join(""))
};
cljs.core.UUID.prototype.cljs$core$IEquiv$_equiv$arity$2 = function(_, other) {
  var this__20629 = this;
  var and__3822__auto____20630 = cljs.core.instance_QMARK_.call(null, cljs.core.UUID, other);
  if(and__3822__auto____20630) {
    return this__20629.uuid === other.uuid
  }else {
    return and__3822__auto____20630
  }
};
cljs.core.UUID.prototype.toString = function() {
  var this__20631 = this;
  var this__20632 = this;
  return cljs.core.pr_str.call(null, this__20632)
};
cljs.core.UUID;
goog.provide("catb.test.navigation");
goog.require("cljs.core");
catb.test.navigation.run = function run() {
  return cljs.core._EQ_.call(null, 4, 4)
};
goog.provide("jayq.util");
goog.require("cljs.core");
jayq.util.map__GT_js = function map__GT_js(m) {
  var out__21089 = {};
  var G__21090__21091 = cljs.core.seq.call(null, m);
  while(true) {
    if(G__21090__21091) {
      var vec__21092__21093 = cljs.core.first.call(null, G__21090__21091);
      var k__21094 = cljs.core.nth.call(null, vec__21092__21093, 0, null);
      var v__21095 = cljs.core.nth.call(null, vec__21092__21093, 1, null);
      out__21089[cljs.core.name.call(null, k__21094)] = v__21095;
      var G__21096 = cljs.core.next.call(null, G__21090__21091);
      G__21090__21091 = G__21096;
      continue
    }else {
    }
    break
  }
  return out__21089
};
jayq.util.wait = function wait(ms, func) {
  return setTimeout(func, ms)
};
jayq.util.log = function() {
  var log__delegate = function(v, text) {
    var vs__21098 = cljs.core.string_QMARK_.call(null, v) ? cljs.core.apply.call(null, cljs.core.str, v, text) : v;
    return console.log(vs__21098)
  };
  var log = function(v, var_args) {
    var text = null;
    if(goog.isDef(var_args)) {
      text = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return log__delegate.call(this, v, text)
  };
  log.cljs$lang$maxFixedArity = 1;
  log.cljs$lang$applyTo = function(arglist__21099) {
    var v = cljs.core.first(arglist__21099);
    var text = cljs.core.rest(arglist__21099);
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
        var obj__21107 = {};
        var G__21108__21109 = cljs.core.seq.call(null, x);
        while(true) {
          if(G__21108__21109) {
            var vec__21110__21111 = cljs.core.first.call(null, G__21108__21109);
            var k__21112 = cljs.core.nth.call(null, vec__21110__21111, 0, null);
            var v__21113 = cljs.core.nth.call(null, vec__21110__21111, 1, null);
            obj__21107[clj__GT_js.call(null, k__21112)] = clj__GT_js.call(null, v__21113);
            var G__21114 = cljs.core.next.call(null, G__21108__21109);
            G__21108__21109 = G__21114;
            continue
          }else {
          }
          break
        }
        return obj__21107
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
goog.provide("catb.backbone.core");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("jayq.util");
catb.backbone.core.IHtml = {};
catb.backbone.core.html = function html(this$) {
  if(function() {
    var and__3822__auto____21011 = this$;
    if(and__3822__auto____21011) {
      return this$.catb$backbone$core$IHtml$html$arity$1
    }else {
      return and__3822__auto____21011
    }
  }()) {
    return this$.catb$backbone$core$IHtml$html$arity$1(this$)
  }else {
    var x__2431__auto____21012 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____21013 = catb.backbone.core.html[goog.typeOf(x__2431__auto____21012)];
      if(or__3824__auto____21013) {
        return or__3824__auto____21013
      }else {
        var or__3824__auto____21014 = catb.backbone.core.html["_"];
        if(or__3824__auto____21014) {
          return or__3824__auto____21014
        }else {
          throw cljs.core.missing_protocol.call(null, "IHtml.html", this$);
        }
      }
    }().call(null, this$)
  }
};
catb.backbone.core.html = function html(this$, content) {
  if(function() {
    var and__3822__auto____21019 = this$;
    if(and__3822__auto____21019) {
      return this$.catb$backbone$core$IHtml$html$arity$2
    }else {
      return and__3822__auto____21019
    }
  }()) {
    return this$.catb$backbone$core$IHtml$html$arity$2(this$, content)
  }else {
    var x__2431__auto____21020 = this$ == null ? null : this$;
    return function() {
      var or__3824__auto____21021 = catb.backbone.core.html[goog.typeOf(x__2431__auto____21020)];
      if(or__3824__auto____21021) {
        return or__3824__auto____21021
      }else {
        var or__3824__auto____21022 = catb.backbone.core.html["_"];
        if(or__3824__auto____21022) {
          return or__3824__auto____21022
        }else {
          throw cljs.core.missing_protocol.call(null, "IHtml.html", this$);
        }
      }
    }().call(null, this$, content)
  }
};
Backbone.View.prototype.catb$backbone$core$IHtml$ = true;
Backbone.View.prototype.catb$backbone$core$IHtml$html$arity$1 = function(this$) {
  return this$.$el.html()
};
Backbone.View.prototype.catb$backbone$core$IHtml$html$arity$2 = function(this$, content) {
  return this$.$el.html(content)
};
catb.backbone.core.get_in_STAR_ = function get_in_STAR_(model, properties) {
  var current__21025 = model;
  var properties__21026 = properties;
  while(true) {
    if(cljs.core.seq.call(null, properties__21026)) {
      var G__21027 = current__21025.get(cljs.core.name.call(null, cljs.core.first.call(null, properties__21026)));
      var G__21028 = cljs.core.rest.call(null, properties__21026);
      current__21025 = G__21027;
      properties__21026 = G__21028;
      continue
    }else {
      return current__21025
    }
    break
  }
};
catb.backbone.core.get_in = function get_in(model, properties) {
  return cljs.core.js__GT_clj.call(null, catb.backbone.core.get_in_STAR_.call(null, model, properties))
};
catb.backbone.core.new$ = function new$(type, attributes) {
  return new type(jayq.util.clj__GT_js.call(null, attributes))
};
catb.backbone.core.new_model = function new_model(attributes) {
  return new Backbone.Model(jayq.util.clj__GT_js.call(null, attributes))
};
catb.backbone.core.render = function render(view) {
  return view.render()
};
catb.backbone.core.save = function save(model, attributes, options) {
  return model.save(jayq.util.clj__GT_js.call(null, attributes), jayq.util.clj__GT_js.call(null, options))
};
goog.provide("catb.i18n");
goog.require("cljs.core");
catb.i18n.i18n = function i18n(k) {
  return jQuery.i18n.prop(k)
};
goog.provide("clojure.set");
goog.require("cljs.core");
clojure.set.bubble_max_key = function bubble_max_key(k, coll) {
  var max__20872 = cljs.core.apply.call(null, cljs.core.max_key, k, coll);
  return cljs.core.cons.call(null, max__20872, cljs.core.remove.call(null, function(p1__20870_SHARP_) {
    return max__20872 === p1__20870_SHARP_
  }, coll))
};
clojure.set.union = function() {
  var union = null;
  var union__0 = function() {
    return cljs.core.PersistentHashSet.EMPTY
  };
  var union__1 = function(s1) {
    return s1
  };
  var union__2 = function(s1, s2) {
    if(cljs.core.count.call(null, s1) < cljs.core.count.call(null, s2)) {
      return cljs.core.reduce.call(null, cljs.core.conj, s2, s1)
    }else {
      return cljs.core.reduce.call(null, cljs.core.conj, s1, s2)
    }
  };
  var union__3 = function() {
    var G__20876__delegate = function(s1, s2, sets) {
      var bubbled_sets__20875 = clojure.set.bubble_max_key.call(null, cljs.core.count, cljs.core.conj.call(null, sets, s2, s1));
      return cljs.core.reduce.call(null, cljs.core.into, cljs.core.first.call(null, bubbled_sets__20875), cljs.core.rest.call(null, bubbled_sets__20875))
    };
    var G__20876 = function(s1, s2, var_args) {
      var sets = null;
      if(goog.isDef(var_args)) {
        sets = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__20876__delegate.call(this, s1, s2, sets)
    };
    G__20876.cljs$lang$maxFixedArity = 2;
    G__20876.cljs$lang$applyTo = function(arglist__20877) {
      var s1 = cljs.core.first(arglist__20877);
      var s2 = cljs.core.first(cljs.core.next(arglist__20877));
      var sets = cljs.core.rest(cljs.core.next(arglist__20877));
      return G__20876__delegate(s1, s2, sets)
    };
    G__20876.cljs$lang$arity$variadic = G__20876__delegate;
    return G__20876
  }();
  union = function(s1, s2, var_args) {
    var sets = var_args;
    switch(arguments.length) {
      case 0:
        return union__0.call(this);
      case 1:
        return union__1.call(this, s1);
      case 2:
        return union__2.call(this, s1, s2);
      default:
        return union__3.cljs$lang$arity$variadic(s1, s2, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  union.cljs$lang$maxFixedArity = 2;
  union.cljs$lang$applyTo = union__3.cljs$lang$applyTo;
  union.cljs$lang$arity$0 = union__0;
  union.cljs$lang$arity$1 = union__1;
  union.cljs$lang$arity$2 = union__2;
  union.cljs$lang$arity$variadic = union__3.cljs$lang$arity$variadic;
  return union
}();
clojure.set.intersection = function() {
  var intersection = null;
  var intersection__1 = function(s1) {
    return s1
  };
  var intersection__2 = function(s1, s2) {
    while(true) {
      if(cljs.core.count.call(null, s2) < cljs.core.count.call(null, s1)) {
        var G__20880 = s2;
        var G__20881 = s1;
        s1 = G__20880;
        s2 = G__20881;
        continue
      }else {
        return cljs.core.reduce.call(null, function(s1, s2) {
          return function(result, item) {
            if(cljs.core.contains_QMARK_.call(null, s2, item)) {
              return result
            }else {
              return cljs.core.disj.call(null, result, item)
            }
          }
        }(s1, s2), s1, s1)
      }
      break
    }
  };
  var intersection__3 = function() {
    var G__20882__delegate = function(s1, s2, sets) {
      var bubbled_sets__20879 = clojure.set.bubble_max_key.call(null, function(p1__20873_SHARP_) {
        return-cljs.core.count.call(null, p1__20873_SHARP_)
      }, cljs.core.conj.call(null, sets, s2, s1));
      return cljs.core.reduce.call(null, intersection, cljs.core.first.call(null, bubbled_sets__20879), cljs.core.rest.call(null, bubbled_sets__20879))
    };
    var G__20882 = function(s1, s2, var_args) {
      var sets = null;
      if(goog.isDef(var_args)) {
        sets = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__20882__delegate.call(this, s1, s2, sets)
    };
    G__20882.cljs$lang$maxFixedArity = 2;
    G__20882.cljs$lang$applyTo = function(arglist__20883) {
      var s1 = cljs.core.first(arglist__20883);
      var s2 = cljs.core.first(cljs.core.next(arglist__20883));
      var sets = cljs.core.rest(cljs.core.next(arglist__20883));
      return G__20882__delegate(s1, s2, sets)
    };
    G__20882.cljs$lang$arity$variadic = G__20882__delegate;
    return G__20882
  }();
  intersection = function(s1, s2, var_args) {
    var sets = var_args;
    switch(arguments.length) {
      case 1:
        return intersection__1.call(this, s1);
      case 2:
        return intersection__2.call(this, s1, s2);
      default:
        return intersection__3.cljs$lang$arity$variadic(s1, s2, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  intersection.cljs$lang$maxFixedArity = 2;
  intersection.cljs$lang$applyTo = intersection__3.cljs$lang$applyTo;
  intersection.cljs$lang$arity$1 = intersection__1;
  intersection.cljs$lang$arity$2 = intersection__2;
  intersection.cljs$lang$arity$variadic = intersection__3.cljs$lang$arity$variadic;
  return intersection
}();
clojure.set.difference = function() {
  var difference = null;
  var difference__1 = function(s1) {
    return s1
  };
  var difference__2 = function(s1, s2) {
    if(cljs.core.count.call(null, s1) < cljs.core.count.call(null, s2)) {
      return cljs.core.reduce.call(null, function(result, item) {
        if(cljs.core.contains_QMARK_.call(null, s2, item)) {
          return cljs.core.disj.call(null, result, item)
        }else {
          return result
        }
      }, s1, s1)
    }else {
      return cljs.core.reduce.call(null, cljs.core.disj, s1, s2)
    }
  };
  var difference__3 = function() {
    var G__20884__delegate = function(s1, s2, sets) {
      return cljs.core.reduce.call(null, difference, s1, cljs.core.conj.call(null, sets, s2))
    };
    var G__20884 = function(s1, s2, var_args) {
      var sets = null;
      if(goog.isDef(var_args)) {
        sets = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
      }
      return G__20884__delegate.call(this, s1, s2, sets)
    };
    G__20884.cljs$lang$maxFixedArity = 2;
    G__20884.cljs$lang$applyTo = function(arglist__20885) {
      var s1 = cljs.core.first(arglist__20885);
      var s2 = cljs.core.first(cljs.core.next(arglist__20885));
      var sets = cljs.core.rest(cljs.core.next(arglist__20885));
      return G__20884__delegate(s1, s2, sets)
    };
    G__20884.cljs$lang$arity$variadic = G__20884__delegate;
    return G__20884
  }();
  difference = function(s1, s2, var_args) {
    var sets = var_args;
    switch(arguments.length) {
      case 1:
        return difference__1.call(this, s1);
      case 2:
        return difference__2.call(this, s1, s2);
      default:
        return difference__3.cljs$lang$arity$variadic(s1, s2, cljs.core.array_seq(arguments, 2))
    }
    throw"Invalid arity: " + arguments.length;
  };
  difference.cljs$lang$maxFixedArity = 2;
  difference.cljs$lang$applyTo = difference__3.cljs$lang$applyTo;
  difference.cljs$lang$arity$1 = difference__1;
  difference.cljs$lang$arity$2 = difference__2;
  difference.cljs$lang$arity$variadic = difference__3.cljs$lang$arity$variadic;
  return difference
}();
clojure.set.select = function select(pred, xset) {
  return cljs.core.reduce.call(null, function(s, k) {
    if(cljs.core.truth_(pred.call(null, k))) {
      return s
    }else {
      return cljs.core.disj.call(null, s, k)
    }
  }, xset, xset)
};
clojure.set.project = function project(xrel, ks) {
  return cljs.core.set.call(null, cljs.core.map.call(null, function(p1__20886_SHARP_) {
    return cljs.core.select_keys.call(null, p1__20886_SHARP_, ks)
  }, xrel))
};
clojure.set.rename_keys = function rename_keys(map, kmap) {
  return cljs.core.reduce.call(null, function(m, p__20894) {
    var vec__20895__20896 = p__20894;
    var old__20897 = cljs.core.nth.call(null, vec__20895__20896, 0, null);
    var new__20898 = cljs.core.nth.call(null, vec__20895__20896, 1, null);
    if(function() {
      var and__3822__auto____20899 = cljs.core.not_EQ_.call(null, old__20897, new__20898);
      if(and__3822__auto____20899) {
        return cljs.core.contains_QMARK_.call(null, m, old__20897)
      }else {
        return and__3822__auto____20899
      }
    }()) {
      return cljs.core.dissoc.call(null, cljs.core.assoc.call(null, m, new__20898, cljs.core._lookup.call(null, m, old__20897, null)), old__20897)
    }else {
      return m
    }
  }, map, kmap)
};
clojure.set.rename = function rename(xrel, kmap) {
  return cljs.core.set.call(null, cljs.core.map.call(null, function(p1__20887_SHARP_) {
    return clojure.set.rename_keys.call(null, p1__20887_SHARP_, kmap)
  }, xrel))
};
clojure.set.index = function index(xrel, ks) {
  return cljs.core.reduce.call(null, function(m, x) {
    var ik__20901 = cljs.core.select_keys.call(null, x, ks);
    return cljs.core.assoc.call(null, m, ik__20901, cljs.core.conj.call(null, cljs.core._lookup.call(null, m, ik__20901, cljs.core.PersistentHashSet.EMPTY), x))
  }, cljs.core.ObjMap.EMPTY, xrel)
};
clojure.set.map_invert = function map_invert(m) {
  return cljs.core.reduce.call(null, function(m, p__20911) {
    var vec__20912__20913 = p__20911;
    var k__20914 = cljs.core.nth.call(null, vec__20912__20913, 0, null);
    var v__20915 = cljs.core.nth.call(null, vec__20912__20913, 1, null);
    return cljs.core.assoc.call(null, m, v__20915, k__20914)
  }, cljs.core.ObjMap.EMPTY, m)
};
clojure.set.join = function() {
  var join = null;
  var join__2 = function(xrel, yrel) {
    if(function() {
      var and__3822__auto____20932 = cljs.core.seq.call(null, xrel);
      if(and__3822__auto____20932) {
        return cljs.core.seq.call(null, yrel)
      }else {
        return and__3822__auto____20932
      }
    }()) {
      var ks__20934 = clojure.set.intersection.call(null, cljs.core.set.call(null, cljs.core.keys.call(null, cljs.core.first.call(null, xrel))), cljs.core.set.call(null, cljs.core.keys.call(null, cljs.core.first.call(null, yrel))));
      var vec__20933__20935 = cljs.core.count.call(null, xrel) <= cljs.core.count.call(null, yrel) ? cljs.core.PersistentVector.fromArray([xrel, yrel], true) : cljs.core.PersistentVector.fromArray([yrel, xrel], true);
      var r__20936 = cljs.core.nth.call(null, vec__20933__20935, 0, null);
      var s__20937 = cljs.core.nth.call(null, vec__20933__20935, 1, null);
      var idx__20938 = clojure.set.index.call(null, r__20936, ks__20934);
      return cljs.core.reduce.call(null, function(ret, x) {
        var found__20939 = idx__20938.call(null, cljs.core.select_keys.call(null, x, ks__20934));
        if(cljs.core.truth_(found__20939)) {
          return cljs.core.reduce.call(null, function(p1__20902_SHARP_, p2__20903_SHARP_) {
            return cljs.core.conj.call(null, p1__20902_SHARP_, cljs.core.merge.call(null, p2__20903_SHARP_, x))
          }, ret, found__20939)
        }else {
          return ret
        }
      }, cljs.core.PersistentHashSet.EMPTY, s__20937)
    }else {
      return cljs.core.PersistentHashSet.EMPTY
    }
  };
  var join__3 = function(xrel, yrel, km) {
    var vec__20940__20941 = cljs.core.count.call(null, xrel) <= cljs.core.count.call(null, yrel) ? cljs.core.PersistentVector.fromArray([xrel, yrel, clojure.set.map_invert.call(null, km)], true) : cljs.core.PersistentVector.fromArray([yrel, xrel, km], true);
    var r__20942 = cljs.core.nth.call(null, vec__20940__20941, 0, null);
    var s__20943 = cljs.core.nth.call(null, vec__20940__20941, 1, null);
    var k__20944 = cljs.core.nth.call(null, vec__20940__20941, 2, null);
    var idx__20945 = clojure.set.index.call(null, r__20942, cljs.core.vals.call(null, k__20944));
    return cljs.core.reduce.call(null, function(ret, x) {
      var found__20946 = idx__20945.call(null, clojure.set.rename_keys.call(null, cljs.core.select_keys.call(null, x, cljs.core.keys.call(null, k__20944)), k__20944));
      if(cljs.core.truth_(found__20946)) {
        return cljs.core.reduce.call(null, function(p1__20904_SHARP_, p2__20905_SHARP_) {
          return cljs.core.conj.call(null, p1__20904_SHARP_, cljs.core.merge.call(null, p2__20905_SHARP_, x))
        }, ret, found__20946)
      }else {
        return ret
      }
    }, cljs.core.PersistentHashSet.EMPTY, s__20943)
  };
  join = function(xrel, yrel, km) {
    switch(arguments.length) {
      case 2:
        return join__2.call(this, xrel, yrel);
      case 3:
        return join__3.call(this, xrel, yrel, km)
    }
    throw"Invalid arity: " + arguments.length;
  };
  join.cljs$lang$arity$2 = join__2;
  join.cljs$lang$arity$3 = join__3;
  return join
}();
clojure.set.subset_QMARK_ = function subset_QMARK_(set1, set2) {
  var and__3822__auto____20949 = cljs.core.count.call(null, set1) <= cljs.core.count.call(null, set2);
  if(and__3822__auto____20949) {
    return cljs.core.every_QMARK_.call(null, function(p1__20916_SHARP_) {
      return cljs.core.contains_QMARK_.call(null, set2, p1__20916_SHARP_)
    }, set1)
  }else {
    return and__3822__auto____20949
  }
};
clojure.set.superset_QMARK_ = function superset_QMARK_(set1, set2) {
  var and__3822__auto____20951 = cljs.core.count.call(null, set1) >= cljs.core.count.call(null, set2);
  if(and__3822__auto____20951) {
    return cljs.core.every_QMARK_.call(null, function(p1__20947_SHARP_) {
      return cljs.core.contains_QMARK_.call(null, set1, p1__20947_SHARP_)
    }, set2)
  }else {
    return and__3822__auto____20951
  }
};
goog.provide("catb.models.similarity");
goog.require("cljs.core");
goog.require("clojure.set");
catb.models.similarity.score = function score(s1, s2) {
  return cljs.core.count.call(null, clojure.set.intersection.call(null, s1, s2)) / cljs.core.count.call(null, s2)
};
catb.models.similarity.value = function value(s1, s2) {
  var s__20958 = catb.models.similarity.score.call(null, s1, s2);
  if(function() {
    var and__3822__auto____20959 = s__20958 >= 0;
    if(and__3822__auto____20959) {
      return s__20958 <= 0.2
    }else {
      return and__3822__auto____20959
    }
  }()) {
    return"\ufdd0'very-little"
  }else {
    if(function() {
      var and__3822__auto____20960 = s__20958 > 0.2;
      if(and__3822__auto____20960) {
        return s__20958 <= 0.4
      }else {
        return and__3822__auto____20960
      }
    }()) {
      return"\ufdd0'little"
    }else {
      if(function() {
        var and__3822__auto____20961 = s__20958 > 0.4;
        if(and__3822__auto____20961) {
          return s__20958 <= 0.6
        }else {
          return and__3822__auto____20961
        }
      }()) {
        return"\ufdd0'some"
      }else {
        if(function() {
          var and__3822__auto____20962 = s__20958 > 0.6;
          if(and__3822__auto____20962) {
            return s__20958 <= 0.8
          }else {
            return and__3822__auto____20962
          }
        }()) {
          return"\ufdd0'much"
        }else {
          if(function() {
            var and__3822__auto____20963 = s__20958 > 0.8;
            if(and__3822__auto____20963) {
              return s__20958 <= 1
            }else {
              return and__3822__auto____20963
            }
          }()) {
            return"\ufdd0'very-much"
          }else {
            return null
          }
        }
      }
    }
  }
};
goog.provide("catb.models.core");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("catb.models.similarity");
goog.require("clojure.set");
goog.require("jayq.util");
catb.models.core.get_stmt = function get_stmt(statements, id) {
  return statements.get(id)
};
catb.models.core.get_arg = function get_arg(args, id) {
  return args.get(id)
};
catb.models.core.get_metadata = function get_metadata(md, k) {
  return cljs.core.first.call(null, cljs.core.filter.call(null, function(m) {
    return cljs.core._EQ_.call(null, m.key, k)
  }, md.toJSON()))
};
catb.models.core.update_statements = function update_statements(m, sources, ids) {
  return cljs.core.reduce.call(null, function(m, source) {
    var v__20851 = cljs.core._lookup.call(null, m, source, cljs.core.PersistentHashSet.EMPTY);
    return cljs.core.assoc.call(null, m, source, clojure.set.union.call(null, v__20851, ids))
  }, m, cljs.core.js__GT_clj.call(null, sources))
};
catb.models.core.statements_by_sources = function statements_by_sources(args) {
  return cljs.core.reduce.call(null, function(m, arg) {
    if(cljs.core.truth_(function() {
      var and__3822__auto____20859 = arg.header;
      if(cljs.core.truth_(and__3822__auto____20859)) {
        return arg.header.source
      }else {
        return and__3822__auto____20859
      }
    }())) {
      var sources__20860 = arg.header.source;
      var premises__20861 = arg.premises;
      var conclusion__20862 = arg.conclusion;
      var conclusion_id__20863 = conclusion__20862.id;
      var premises_ids__20864 = cljs.core.set.call(null, cljs.core.map.call(null, function(p) {
        return p.statement.id
      }, premises__20861));
      var ids__20865 = cljs.core.truth_(arg.pro) ? cljs.core.conj.call(null, premises_ids__20864, conclusion_id__20863) : premises_ids__20864;
      return catb.models.core.update_statements.call(null, m, sources__20860, ids__20865)
    }else {
      return m
    }
  }, cljs.core.ObjMap.EMPTY, args)
};
catb.models.core.sources_by_similarity = function sources_by_similarity(statements, accepted) {
  return cljs.core.group_by.call(null, function(source) {
    return catb.models.similarity.value.call(null, accepted, statements.call(null, source))
  }, cljs.core.keys.call(null, statements))
};
catb.models.core.arguments_for_statement = function arguments_for_statement(issue, args) {
  var pro_statements__20868 = cljs.core.mapcat.call(null, function(proid) {
    return cljs.core.map.call(null, function(p) {
      return p.statement
    }, args.get(proid).get("premises"))
  }, issue.pro);
  var con_statements__20869 = cljs.core.mapcat.call(null, function(conid) {
    return cljs.core.map.call(null, function(p) {
      return p.statement
    }, args.get(conid).get("premises"))
  }, issue.con);
  return cljs.core.concat.call(null, cljs.core.map.call(null, cljs.core.partial.call(null, catb.models.core.get_arg, args), issue.pro), cljs.core.map.call(null, cljs.core.partial.call(null, catb.models.core.get_arg, args), issue.con), cljs.core.mapcat.call(null, function(s) {
    return arguments_for_statement.call(null, s, args)
  }, pro_statements__20868), cljs.core.mapcat.call(null, function(s) {
    return arguments_for_statement.call(null, s, args)
  }, con_statements__20869))
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
      var s__20978 = s;
      var limit__20979 = limit;
      var parts__20980 = cljs.core.PersistentVector.EMPTY;
      while(true) {
        if(cljs.core._EQ_.call(null, limit__20979, 1)) {
          return cljs.core.conj.call(null, parts__20980, s__20978)
        }else {
          var temp__3971__auto____20981 = cljs.core.re_find.call(null, re, s__20978);
          if(cljs.core.truth_(temp__3971__auto____20981)) {
            var m__20982 = temp__3971__auto____20981;
            var index__20983 = s__20978.indexOf(m__20982);
            var G__20984 = s__20978.substring(index__20983 + cljs.core.count.call(null, m__20982));
            var G__20985 = limit__20979 - 1;
            var G__20986 = cljs.core.conj.call(null, parts__20980, s__20978.substring(0, index__20983));
            s__20978 = G__20984;
            limit__20979 = G__20985;
            parts__20980 = G__20986;
            continue
          }else {
            return cljs.core.conj.call(null, parts__20980, s__20978)
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
  var index__20990 = s.length;
  while(true) {
    if(index__20990 === 0) {
      return""
    }else {
      var ch__20991 = cljs.core._lookup.call(null, s, index__20990 - 1, null);
      if(function() {
        var or__3824__auto____20992 = cljs.core._EQ_.call(null, ch__20991, "\n");
        if(or__3824__auto____20992) {
          return or__3824__auto____20992
        }else {
          return cljs.core._EQ_.call(null, ch__20991, "\r")
        }
      }()) {
        var G__20993 = index__20990 - 1;
        index__20990 = G__20993;
        continue
      }else {
        return s.substring(0, index__20990)
      }
    }
    break
  }
};
clojure.string.blank_QMARK_ = function blank_QMARK_(s) {
  return goog.string.isEmptySafe(s)
};
clojure.string.escape = function escape(s, cmap) {
  var buffer__21000 = new goog.string.StringBuffer;
  var length__21001 = s.length;
  var index__21002 = 0;
  while(true) {
    if(cljs.core._EQ_.call(null, length__21001, index__21002)) {
      return buffer__21000.toString()
    }else {
      var ch__21003 = s.charAt(index__21002);
      var temp__3971__auto____21004 = cljs.core._lookup.call(null, cmap, ch__21003, null);
      if(cljs.core.truth_(temp__3971__auto____21004)) {
        var replacement__21005 = temp__3971__auto____21004;
        buffer__21000.append([cljs.core.str(replacement__21005)].join(""))
      }else {
        buffer__21000.append(ch__21003)
      }
      var G__21006 = index__21002 + 1;
      index__21002 = G__21006;
      continue
    }
    break
  }
};
goog.provide("catb.icanhaz.core");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("clojure.string");
goog.require("jayq.util");
catb.icanhaz.core.get = function get(template_key, variables) {
  var tname__20971 = clojure.string.replace.call(null, jayq.util.clj__GT_js.call(null, template_key), "-", "_");
  return ich[tname__20971].call(ich, jayq.util.clj__GT_js.call(null, variables))
};
goog.provide("catb.template");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("catb.icanhaz.core");
goog.require("jayq.util");
catb.template.get = function get(template, variables) {
  var entries__20967 = cljs.core.keys.call(null, cljs.core.js__GT_clj.call(null, jQuery.i18n.map));
  var texts__20968 = cljs.core.apply.call(null, cljs.core.hash_map, cljs.core.mapcat.call(null, function(e) {
    return cljs.core.PersistentVector.fromArray([cljs.core.keyword.call(null, e), jQuery.i18n.prop(e)], true)
  }, entries__20967));
  var augmented__20969 = cljs.core.merge.call(null, texts__20968, variables);
  return catb.icanhaz.core.get.call(null, template, augmented__20969)
};
goog.provide("catb.views.core");
goog.require("cljs.core");
goog.require("catb.models.core");
goog.require("jayq.util");
goog.require("catb.template");
goog.require("catb.backbone.core");
goog.require("catb.models.core");
goog.require("jayq.util");
catb.views.core.template = function template(view, template_key, variables) {
  return catb.backbone.core.html.call(null, view, catb.template.get.call(null, template_key, variables))
};
catb.views.core.json = function json(model) {
  return model.toJSON()
};
catb.views.core.score_agreed_QMARK_ = function score_agreed_QMARK_(score) {
  return score > 0.99
};
catb.views.core.agreed_QMARK_ = function agreed_QMARK_(claim, votes) {
  var vote__20827 = votes.call(null, claim.id);
  var and__3822__auto____20828 = !(vote__20827 == null);
  if(and__3822__auto____20828) {
    return catb.views.core.score_agreed_QMARK_.call(null, vote__20827)
  }else {
    return and__3822__auto____20828
  }
};
catb.views.core.disagreed_QMARK_ = function disagreed_QMARK_(claim, votes) {
  var vote__20831 = votes.call(null, claim.id);
  var and__3822__auto____20832 = !(vote__20831 == null);
  if(and__3822__auto____20832) {
    return vote__20831 < 0.01
  }else {
    return and__3822__auto____20832
  }
};
catb.views.core.prepare_claim = function prepare_claim(statement_votes, stmt) {
  var text__20836 = AGB.statement_raw_text(stmt);
  var desc__20837 = AGB.description_text(stmt.header);
  var agreement_text__20838 = cljs.core.truth_(catb.views.core.agreed_QMARK_.call(null, stmt, statement_votes)) ? jQuery.i18n.prop("sct_agree") : cljs.core.truth_(catb.views.core.disagreed_QMARK_.call(null, stmt, statement_votes)) ? jQuery.i18n.prop("sct_disagree") : "\ufdd0'else" ? "" : null;
  stmt["statement_text"] = text__20836;
  stmt["statement_description"] = desc__20837;
  stmt["agreement_text"] = agreement_text__20838;
  return stmt
};
catb.views.core.prepare_argument = function prepare_argument(arg) {
  return arg["description_text"] = AGB.description_text(arg.header)
};
catb.views.core.pro_answered = function pro_answered(claim, args, argument_votes) {
  return cljs.core.map.call(null, cljs.core.comp.call(null, catb.views.core.json, cljs.core.partial.call(null, catb.models.core.get_arg, args)), cljs.core.filter.call(null, function(id) {
    return argument_votes.call(null, id)
  }, claim.pro))
};
catb.views.core.con_answered = function con_answered(claim, args, argument_votes) {
  return cljs.core.map.call(null, cljs.core.comp.call(null, catb.views.core.json, cljs.core.partial.call(null, catb.models.core.get_arg, args)), cljs.core.filter.call(null, function(id) {
    return argument_votes.call(null, id)
  }, claim.con))
};
catb.views.core.prepare_arguments = function prepare_arguments(claim, args, argument_votes) {
  var pro_answered__20844 = catb.views.core.pro_answered.call(null, claim, args, argument_votes);
  var con_answered__20845 = catb.views.core.con_answered.call(null, claim, args, argument_votes);
  var G__20846__20847 = cljs.core.seq.call(null, cljs.core.concat.call(null, pro_answered__20844, con_answered__20845));
  while(true) {
    if(G__20846__20847) {
      var arg__20848 = cljs.core.first.call(null, G__20846__20847);
      catb.views.core.prepare_argument.call(null, arg__20848);
      var G__20849 = cljs.core.next.call(null, G__20846__20847);
      G__20846__20847 = G__20849;
      continue
    }else {
    }
    break
  }
  claim["has_pro_answered"] = cljs.core.count.call(null, pro_answered__20844) > 0;
  claim["has_con_answered"] = cljs.core.count.call(null, con_answered__20845) > 0;
  claim["pro_answered"] = jayq.util.clj__GT_js.call(null, pro_answered__20844);
  claim["con_answered"] = jayq.util.clj__GT_js.call(null, con_answered__20845);
  return claim
};
goog.provide("catb.dispatch");
goog.require("cljs.core");
catb.dispatch.reactions = cljs.core.atom.call(null, cljs.core.ObjMap.EMPTY);
catb.dispatch.react_to = function() {
  var react_to = null;
  var react_to__2 = function(event_pred, reactor) {
    return react_to.call(null, null, event_pred, reactor)
  };
  var react_to__3 = function(max_count, event_pred, reactor) {
    var reaction__21030 = cljs.core.ObjMap.fromObject(["\ufdd0'max-count", "\ufdd0'event-pred", "\ufdd0'reactor"], {"\ufdd0'max-count":max_count, "\ufdd0'event-pred":event_pred, "\ufdd0'reactor":reactor});
    cljs.core.swap_BANG_.call(null, catb.dispatch.reactions, cljs.core.assoc, reaction__21030, 0);
    return reaction__21030
  };
  react_to = function(max_count, event_pred, reactor) {
    switch(arguments.length) {
      case 2:
        return react_to__2.call(this, max_count, event_pred);
      case 3:
        return react_to__3.call(this, max_count, event_pred, reactor)
    }
    throw"Invalid arity: " + arguments.length;
  };
  react_to.cljs$lang$arity$2 = react_to__2;
  react_to.cljs$lang$arity$3 = react_to__3;
  return react_to
}();
catb.dispatch.delete_reaction = function delete_reaction(reaction) {
  return cljs.core.swap_BANG_.call(null, catb.dispatch.reactions, cljs.core.dissoc, reaction)
};
catb.dispatch.fire = function() {
  var fire = null;
  var fire__1 = function(event_id) {
    return fire.call(null, event_id, null)
  };
  var fire__2 = function(event_id, event_data) {
    var matching_reactions__21067 = cljs.core.filter.call(null, function(p__21059) {
      var vec__21060__21062 = p__21059;
      var map__21061__21063 = cljs.core.nth.call(null, vec__21060__21062, 0, null);
      var map__21061__21064 = cljs.core.seq_QMARK_.call(null, map__21061__21063) ? cljs.core.apply.call(null, cljs.core.hash_map, map__21061__21063) : map__21061__21063;
      var event_pred__21065 = cljs.core._lookup.call(null, map__21061__21064, "\ufdd0'event-pred", null);
      var run_count__21066 = cljs.core.nth.call(null, vec__21060__21062, 1, null);
      return event_pred__21065.call(null, event_id)
    }, cljs.core.deref.call(null, catb.dispatch.reactions));
    var G__21068__21069 = cljs.core.seq.call(null, matching_reactions__21067);
    while(true) {
      if(G__21068__21069) {
        var vec__21070__21071 = cljs.core.first.call(null, G__21068__21069);
        var reaction__21072 = cljs.core.nth.call(null, vec__21070__21071, 0, null);
        var run_count__21073 = cljs.core.nth.call(null, vec__21070__21071, 1, null);
        var map__21074__21075 = reaction__21072;
        var map__21074__21076 = cljs.core.seq_QMARK_.call(null, map__21074__21075) ? cljs.core.apply.call(null, cljs.core.hash_map, map__21074__21075) : map__21074__21075;
        var reactor__21077 = cljs.core._lookup.call(null, map__21074__21076, "\ufdd0'reactor", null);
        var max_count__21078 = cljs.core._lookup.call(null, map__21074__21076, "\ufdd0'max-count", null);
        var run_count__21079 = run_count__21073 + 1;
        reactor__21077.call(null, event_id, event_data);
        if(cljs.core.truth_(function() {
          var and__3822__auto____21080 = max_count__21078;
          if(cljs.core.truth_(and__3822__auto____21080)) {
            return max_count__21078 <= run_count__21079
          }else {
            return and__3822__auto____21080
          }
        }())) {
          catb.dispatch.delete_reaction.call(null, reaction__21072)
        }else {
          cljs.core.swap_BANG_.call(null, catb.dispatch.reactions, cljs.core.assoc, reaction__21072, run_count__21079)
        }
        var G__21081 = cljs.core.next.call(null, G__21068__21069);
        G__21068__21069 = G__21081;
        continue
      }else {
        return null
      }
      break
    }
  };
  fire = function(event_id, event_data) {
    switch(arguments.length) {
      case 1:
        return fire__1.call(this, event_id);
      case 2:
        return fire__2.call(this, event_id, event_data)
    }
    throw"Invalid arity: " + arguments.length;
  };
  fire.cljs$lang$arity$1 = fire__1;
  fire.cljs$lang$arity$2 = fire__2;
  return fire
}();
goog.provide("cljs.reader");
goog.require("cljs.core");
goog.require("goog.string");
cljs.reader.PushbackReader = {};
cljs.reader.read_char = function read_char(reader) {
  if(function() {
    var and__3822__auto____21301 = reader;
    if(and__3822__auto____21301) {
      return reader.cljs$reader$PushbackReader$read_char$arity$1
    }else {
      return and__3822__auto____21301
    }
  }()) {
    return reader.cljs$reader$PushbackReader$read_char$arity$1(reader)
  }else {
    var x__2431__auto____21302 = reader == null ? null : reader;
    return function() {
      var or__3824__auto____21303 = cljs.reader.read_char[goog.typeOf(x__2431__auto____21302)];
      if(or__3824__auto____21303) {
        return or__3824__auto____21303
      }else {
        var or__3824__auto____21304 = cljs.reader.read_char["_"];
        if(or__3824__auto____21304) {
          return or__3824__auto____21304
        }else {
          throw cljs.core.missing_protocol.call(null, "PushbackReader.read-char", reader);
        }
      }
    }().call(null, reader)
  }
};
cljs.reader.unread = function unread(reader, ch) {
  if(function() {
    var and__3822__auto____21309 = reader;
    if(and__3822__auto____21309) {
      return reader.cljs$reader$PushbackReader$unread$arity$2
    }else {
      return and__3822__auto____21309
    }
  }()) {
    return reader.cljs$reader$PushbackReader$unread$arity$2(reader, ch)
  }else {
    var x__2431__auto____21310 = reader == null ? null : reader;
    return function() {
      var or__3824__auto____21311 = cljs.reader.unread[goog.typeOf(x__2431__auto____21310)];
      if(or__3824__auto____21311) {
        return or__3824__auto____21311
      }else {
        var or__3824__auto____21312 = cljs.reader.unread["_"];
        if(or__3824__auto____21312) {
          return or__3824__auto____21312
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
  var this__21313 = this;
  if(cljs.core.empty_QMARK_.call(null, cljs.core.deref.call(null, this__21313.buffer_atom))) {
    var idx__21314 = cljs.core.deref.call(null, this__21313.index_atom);
    cljs.core.swap_BANG_.call(null, this__21313.index_atom, cljs.core.inc);
    return this__21313.s[idx__21314]
  }else {
    var buf__21315 = cljs.core.deref.call(null, this__21313.buffer_atom);
    cljs.core.swap_BANG_.call(null, this__21313.buffer_atom, cljs.core.rest);
    return cljs.core.first.call(null, buf__21315)
  }
};
cljs.reader.StringPushbackReader.prototype.cljs$reader$PushbackReader$unread$arity$2 = function(reader, ch) {
  var this__21316 = this;
  return cljs.core.swap_BANG_.call(null, this__21316.buffer_atom, function(p1__21296_SHARP_) {
    return cljs.core.cons.call(null, ch, p1__21296_SHARP_)
  })
};
cljs.reader.StringPushbackReader;
cljs.reader.push_back_reader = function push_back_reader(s) {
  return new cljs.reader.StringPushbackReader(s, cljs.core.atom.call(null, 0), cljs.core.atom.call(null, null))
};
cljs.reader.whitespace_QMARK_ = function whitespace_QMARK_(ch) {
  var or__3824__auto____21318 = goog.string.isBreakingWhitespace(ch);
  if(cljs.core.truth_(or__3824__auto____21318)) {
    return or__3824__auto____21318
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
  var or__3824__auto____21323 = cljs.reader.numeric_QMARK_.call(null, initch);
  if(or__3824__auto____21323) {
    return or__3824__auto____21323
  }else {
    var and__3822__auto____21325 = function() {
      var or__3824__auto____21324 = "+" === initch;
      if(or__3824__auto____21324) {
        return or__3824__auto____21324
      }else {
        return"-" === initch
      }
    }();
    if(cljs.core.truth_(and__3822__auto____21325)) {
      return cljs.reader.numeric_QMARK_.call(null, function() {
        var next_ch__21326 = cljs.reader.read_char.call(null, reader);
        cljs.reader.unread.call(null, reader, next_ch__21326);
        return next_ch__21326
      }())
    }else {
      return and__3822__auto____21325
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
  reader_error.cljs$lang$applyTo = function(arglist__21327) {
    var rdr = cljs.core.first(arglist__21327);
    var msg = cljs.core.rest(arglist__21327);
    return reader_error__delegate(rdr, msg)
  };
  reader_error.cljs$lang$arity$variadic = reader_error__delegate;
  return reader_error
}();
cljs.reader.macro_terminating_QMARK_ = function macro_terminating_QMARK_(ch) {
  var and__3822__auto____21331 = !(ch === "#");
  if(and__3822__auto____21331) {
    var and__3822__auto____21332 = !(ch === "'");
    if(and__3822__auto____21332) {
      var and__3822__auto____21333 = !(ch === ":");
      if(and__3822__auto____21333) {
        return cljs.reader.macros.call(null, ch)
      }else {
        return and__3822__auto____21333
      }
    }else {
      return and__3822__auto____21332
    }
  }else {
    return and__3822__auto____21331
  }
};
cljs.reader.read_token = function read_token(rdr, initch) {
  var sb__21338 = new goog.string.StringBuffer(initch);
  var ch__21339 = cljs.reader.read_char.call(null, rdr);
  while(true) {
    if(function() {
      var or__3824__auto____21340 = ch__21339 == null;
      if(or__3824__auto____21340) {
        return or__3824__auto____21340
      }else {
        var or__3824__auto____21341 = cljs.reader.whitespace_QMARK_.call(null, ch__21339);
        if(or__3824__auto____21341) {
          return or__3824__auto____21341
        }else {
          return cljs.reader.macro_terminating_QMARK_.call(null, ch__21339)
        }
      }
    }()) {
      cljs.reader.unread.call(null, rdr, ch__21339);
      return sb__21338.toString()
    }else {
      var G__21342 = function() {
        sb__21338.append(ch__21339);
        return sb__21338
      }();
      var G__21343 = cljs.reader.read_char.call(null, rdr);
      sb__21338 = G__21342;
      ch__21339 = G__21343;
      continue
    }
    break
  }
};
cljs.reader.skip_line = function skip_line(reader, _) {
  while(true) {
    var ch__21347 = cljs.reader.read_char.call(null, reader);
    if(function() {
      var or__3824__auto____21348 = ch__21347 === "n";
      if(or__3824__auto____21348) {
        return or__3824__auto____21348
      }else {
        var or__3824__auto____21349 = ch__21347 === "r";
        if(or__3824__auto____21349) {
          return or__3824__auto____21349
        }else {
          return ch__21347 == null
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
  var matches__21351 = re.exec(s);
  if(matches__21351 == null) {
    return null
  }else {
    if(matches__21351.length === 1) {
      return matches__21351[0]
    }else {
      return matches__21351
    }
  }
};
cljs.reader.match_int = function match_int(s) {
  var groups__21359 = cljs.reader.re_find_STAR_.call(null, cljs.reader.int_pattern, s);
  var group3__21360 = groups__21359[2];
  if(!function() {
    var or__3824__auto____21361 = group3__21360 == null;
    if(or__3824__auto____21361) {
      return or__3824__auto____21361
    }else {
      return group3__21360.length < 1
    }
  }()) {
    return 0
  }else {
    var negate__21362 = "-" === groups__21359[1] ? -1 : 1;
    var a__21363 = cljs.core.truth_(groups__21359[3]) ? [groups__21359[3], 10] : cljs.core.truth_(groups__21359[4]) ? [groups__21359[4], 16] : cljs.core.truth_(groups__21359[5]) ? [groups__21359[5], 8] : cljs.core.truth_(groups__21359[7]) ? [groups__21359[7], parseInt(groups__21359[7])] : "\ufdd0'default" ? [null, null] : null;
    var n__21364 = a__21363[0];
    var radix__21365 = a__21363[1];
    if(n__21364 == null) {
      return null
    }else {
      return negate__21362 * parseInt(n__21364, radix__21365)
    }
  }
};
cljs.reader.match_ratio = function match_ratio(s) {
  var groups__21369 = cljs.reader.re_find_STAR_.call(null, cljs.reader.ratio_pattern, s);
  var numinator__21370 = groups__21369[1];
  var denominator__21371 = groups__21369[2];
  return parseInt(numinator__21370) / parseInt(denominator__21371)
};
cljs.reader.match_float = function match_float(s) {
  return parseFloat(s)
};
cljs.reader.re_matches_STAR_ = function re_matches_STAR_(re, s) {
  var matches__21374 = re.exec(s);
  if(function() {
    var and__3822__auto____21375 = !(matches__21374 == null);
    if(and__3822__auto____21375) {
      return matches__21374[0] === s
    }else {
      return and__3822__auto____21375
    }
  }()) {
    if(matches__21374.length === 1) {
      return matches__21374[0]
    }else {
      return matches__21374
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
  var code__21377 = parseInt(code_str, 16);
  return String.fromCharCode(code__21377)
};
cljs.reader.escape_char = function escape_char(buffer, reader) {
  var ch__21380 = cljs.reader.read_char.call(null, reader);
  var mapresult__21381 = cljs.reader.escape_char_map.call(null, ch__21380);
  if(cljs.core.truth_(mapresult__21381)) {
    return mapresult__21381
  }else {
    if(ch__21380 === "x") {
      return cljs.reader.make_unicode_char.call(null, cljs.reader.validate_unicode_escape.call(null, cljs.reader.unicode_2_pattern, reader, ch__21380, cljs.reader.read_2_chars.call(null, reader)))
    }else {
      if(ch__21380 === "u") {
        return cljs.reader.make_unicode_char.call(null, cljs.reader.validate_unicode_escape.call(null, cljs.reader.unicode_4_pattern, reader, ch__21380, cljs.reader.read_4_chars.call(null, reader)))
      }else {
        if(cljs.reader.numeric_QMARK_.call(null, ch__21380)) {
          return String.fromCharCode(ch__21380)
        }else {
          if("\ufdd0'else") {
            return cljs.reader.reader_error.call(null, reader, "Unexpected unicode escape \\", ch__21380)
          }else {
            return null
          }
        }
      }
    }
  }
};
cljs.reader.read_past = function read_past(pred, rdr) {
  var ch__21383 = cljs.reader.read_char.call(null, rdr);
  while(true) {
    if(cljs.core.truth_(pred.call(null, ch__21383))) {
      var G__21384 = cljs.reader.read_char.call(null, rdr);
      ch__21383 = G__21384;
      continue
    }else {
      return ch__21383
    }
    break
  }
};
cljs.reader.read_delimited_list = function read_delimited_list(delim, rdr, recursive_QMARK_) {
  var a__21391 = cljs.core.transient$.call(null, cljs.core.PersistentVector.EMPTY);
  while(true) {
    var ch__21392 = cljs.reader.read_past.call(null, cljs.reader.whitespace_QMARK_, rdr);
    if(cljs.core.truth_(ch__21392)) {
    }else {
      cljs.reader.reader_error.call(null, rdr, "EOF while reading")
    }
    if(delim === ch__21392) {
      return cljs.core.persistent_BANG_.call(null, a__21391)
    }else {
      var temp__3971__auto____21393 = cljs.reader.macros.call(null, ch__21392);
      if(cljs.core.truth_(temp__3971__auto____21393)) {
        var macrofn__21394 = temp__3971__auto____21393;
        var mret__21395 = macrofn__21394.call(null, rdr, ch__21392);
        var G__21397 = mret__21395 === rdr ? a__21391 : cljs.core.conj_BANG_.call(null, a__21391, mret__21395);
        a__21391 = G__21397;
        continue
      }else {
        cljs.reader.unread.call(null, rdr, ch__21392);
        var o__21396 = cljs.reader.read.call(null, rdr, true, null, recursive_QMARK_);
        var G__21398 = o__21396 === rdr ? a__21391 : cljs.core.conj_BANG_.call(null, a__21391, o__21396);
        a__21391 = G__21398;
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
  var ch__21403 = cljs.reader.read_char.call(null, rdr);
  var dm__21404 = cljs.reader.dispatch_macros.call(null, ch__21403);
  if(cljs.core.truth_(dm__21404)) {
    return dm__21404.call(null, rdr, _)
  }else {
    var temp__3971__auto____21405 = cljs.reader.maybe_read_tagged_type.call(null, rdr, ch__21403);
    if(cljs.core.truth_(temp__3971__auto____21405)) {
      var obj__21406 = temp__3971__auto____21405;
      return obj__21406
    }else {
      return cljs.reader.reader_error.call(null, rdr, "No dispatch macro for ", ch__21403)
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
  var l__21408 = cljs.reader.read_delimited_list.call(null, "}", rdr, true);
  if(cljs.core.odd_QMARK_.call(null, cljs.core.count.call(null, l__21408))) {
    cljs.reader.reader_error.call(null, rdr, "Map literal must contain an even number of forms")
  }else {
  }
  return cljs.core.apply.call(null, cljs.core.hash_map, l__21408)
};
cljs.reader.read_number = function read_number(reader, initch) {
  var buffer__21415 = new goog.string.StringBuffer(initch);
  var ch__21416 = cljs.reader.read_char.call(null, reader);
  while(true) {
    if(cljs.core.truth_(function() {
      var or__3824__auto____21417 = ch__21416 == null;
      if(or__3824__auto____21417) {
        return or__3824__auto____21417
      }else {
        var or__3824__auto____21418 = cljs.reader.whitespace_QMARK_.call(null, ch__21416);
        if(or__3824__auto____21418) {
          return or__3824__auto____21418
        }else {
          return cljs.reader.macros.call(null, ch__21416)
        }
      }
    }())) {
      cljs.reader.unread.call(null, reader, ch__21416);
      var s__21419 = buffer__21415.toString();
      var or__3824__auto____21420 = cljs.reader.match_number.call(null, s__21419);
      if(cljs.core.truth_(or__3824__auto____21420)) {
        return or__3824__auto____21420
      }else {
        return cljs.reader.reader_error.call(null, reader, "Invalid number format [", s__21419, "]")
      }
    }else {
      var G__21421 = function() {
        buffer__21415.append(ch__21416);
        return buffer__21415
      }();
      var G__21422 = cljs.reader.read_char.call(null, reader);
      buffer__21415 = G__21421;
      ch__21416 = G__21422;
      continue
    }
    break
  }
};
cljs.reader.read_string_STAR_ = function read_string_STAR_(reader, _) {
  var buffer__21425 = new goog.string.StringBuffer;
  var ch__21426 = cljs.reader.read_char.call(null, reader);
  while(true) {
    if(ch__21426 == null) {
      return cljs.reader.reader_error.call(null, reader, "EOF while reading")
    }else {
      if("\\" === ch__21426) {
        var G__21427 = function() {
          buffer__21425.append(cljs.reader.escape_char.call(null, buffer__21425, reader));
          return buffer__21425
        }();
        var G__21428 = cljs.reader.read_char.call(null, reader);
        buffer__21425 = G__21427;
        ch__21426 = G__21428;
        continue
      }else {
        if('"' === ch__21426) {
          return buffer__21425.toString()
        }else {
          if("\ufdd0'default") {
            var G__21429 = function() {
              buffer__21425.append(ch__21426);
              return buffer__21425
            }();
            var G__21430 = cljs.reader.read_char.call(null, reader);
            buffer__21425 = G__21429;
            ch__21426 = G__21430;
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
  var token__21432 = cljs.reader.read_token.call(null, reader, initch);
  if(cljs.core.truth_(goog.string.contains(token__21432, "/"))) {
    return cljs.core.symbol.call(null, cljs.core.subs.call(null, token__21432, 0, token__21432.indexOf("/")), cljs.core.subs.call(null, token__21432, token__21432.indexOf("/") + 1, token__21432.length))
  }else {
    return cljs.reader.special_symbols.call(null, token__21432, cljs.core.symbol.call(null, token__21432))
  }
};
cljs.reader.read_keyword = function read_keyword(reader, initch) {
  var token__21442 = cljs.reader.read_token.call(null, reader, cljs.reader.read_char.call(null, reader));
  var a__21443 = cljs.reader.re_matches_STAR_.call(null, cljs.reader.symbol_pattern, token__21442);
  var token__21444 = a__21443[0];
  var ns__21445 = a__21443[1];
  var name__21446 = a__21443[2];
  if(cljs.core.truth_(function() {
    var or__3824__auto____21448 = function() {
      var and__3822__auto____21447 = !(void 0 === ns__21445);
      if(and__3822__auto____21447) {
        return ns__21445.substring(ns__21445.length - 2, ns__21445.length) === ":/"
      }else {
        return and__3822__auto____21447
      }
    }();
    if(cljs.core.truth_(or__3824__auto____21448)) {
      return or__3824__auto____21448
    }else {
      var or__3824__auto____21449 = name__21446[name__21446.length - 1] === ":";
      if(or__3824__auto____21449) {
        return or__3824__auto____21449
      }else {
        return!(token__21444.indexOf("::", 1) === -1)
      }
    }
  }())) {
    return cljs.reader.reader_error.call(null, reader, "Invalid token: ", token__21444)
  }else {
    if(function() {
      var and__3822__auto____21450 = !(ns__21445 == null);
      if(and__3822__auto____21450) {
        return ns__21445.length > 0
      }else {
        return and__3822__auto____21450
      }
    }()) {
      return cljs.core.keyword.call(null, ns__21445.substring(0, ns__21445.indexOf("/")), name__21446)
    }else {
      return cljs.core.keyword.call(null, token__21444)
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
  var m__21456 = cljs.reader.desugar_meta.call(null, cljs.reader.read.call(null, rdr, true, null, true));
  if(cljs.core.map_QMARK_.call(null, m__21456)) {
  }else {
    cljs.reader.reader_error.call(null, rdr, "Metadata must be Symbol,Keyword,String or Map")
  }
  var o__21457 = cljs.reader.read.call(null, rdr, true, null, true);
  if(function() {
    var G__21458__21459 = o__21457;
    if(G__21458__21459) {
      if(function() {
        var or__3824__auto____21460 = G__21458__21459.cljs$lang$protocol_mask$partition0$ & 262144;
        if(or__3824__auto____21460) {
          return or__3824__auto____21460
        }else {
          return G__21458__21459.cljs$core$IWithMeta$
        }
      }()) {
        return true
      }else {
        if(!G__21458__21459.cljs$lang$protocol_mask$partition0$) {
          return cljs.core.type_satisfies_.call(null, cljs.core.IWithMeta, G__21458__21459)
        }else {
          return false
        }
      }
    }else {
      return cljs.core.type_satisfies_.call(null, cljs.core.IWithMeta, G__21458__21459)
    }
  }()) {
    return cljs.core.with_meta.call(null, o__21457, cljs.core.merge.call(null, cljs.core.meta.call(null, o__21457), m__21456))
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
    var ch__21464 = cljs.reader.read_char.call(null, reader);
    if(ch__21464 == null) {
      if(cljs.core.truth_(eof_is_error)) {
        return cljs.reader.reader_error.call(null, reader, "EOF while reading")
      }else {
        return sentinel
      }
    }else {
      if(cljs.reader.whitespace_QMARK_.call(null, ch__21464)) {
        var G__21467 = reader;
        var G__21468 = eof_is_error;
        var G__21469 = sentinel;
        var G__21470 = is_recursive;
        reader = G__21467;
        eof_is_error = G__21468;
        sentinel = G__21469;
        is_recursive = G__21470;
        continue
      }else {
        if(cljs.reader.comment_prefix_QMARK_.call(null, ch__21464)) {
          var G__21471 = cljs.reader.read_comment.call(null, reader, ch__21464);
          var G__21472 = eof_is_error;
          var G__21473 = sentinel;
          var G__21474 = is_recursive;
          reader = G__21471;
          eof_is_error = G__21472;
          sentinel = G__21473;
          is_recursive = G__21474;
          continue
        }else {
          if("\ufdd0'else") {
            var f__21465 = cljs.reader.macros.call(null, ch__21464);
            var res__21466 = cljs.core.truth_(f__21465) ? f__21465.call(null, reader, ch__21464) : cljs.reader.number_literal_QMARK_.call(null, reader, ch__21464) ? cljs.reader.read_number.call(null, reader, ch__21464) : "\ufdd0'else" ? cljs.reader.read_symbol.call(null, reader, ch__21464) : null;
            if(res__21466 === reader) {
              var G__21475 = reader;
              var G__21476 = eof_is_error;
              var G__21477 = sentinel;
              var G__21478 = is_recursive;
              reader = G__21475;
              eof_is_error = G__21476;
              sentinel = G__21477;
              is_recursive = G__21478;
              continue
            }else {
              return res__21466
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
  var r__21480 = cljs.reader.push_back_reader.call(null, s);
  return cljs.reader.read.call(null, r__21480, true, null, false)
};
cljs.reader.zero_fill_right = function zero_fill_right(s, width) {
  if(cljs.core._EQ_.call(null, width, cljs.core.count.call(null, s))) {
    return s
  }else {
    if(width < cljs.core.count.call(null, s)) {
      return s.substring(0, width)
    }else {
      if("\ufdd0'else") {
        var b__21482 = new goog.string.StringBuffer(s);
        while(true) {
          if(b__21482.getLength() < width) {
            var G__21483 = b__21482.append("0");
            b__21482 = G__21483;
            continue
          }else {
            return b__21482.toString()
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
  var and__3822__auto____21486 = cljs.reader.divisible_QMARK_.call(null, year, 4);
  if(cljs.core.truth_(and__3822__auto____21486)) {
    var or__3824__auto____21487 = cljs.reader.indivisible_QMARK_.call(null, year, 100);
    if(cljs.core.truth_(or__3824__auto____21487)) {
      return or__3824__auto____21487
    }else {
      return cljs.reader.divisible_QMARK_.call(null, year, 400)
    }
  }else {
    return and__3822__auto____21486
  }
};
cljs.reader.days_in_month = function() {
  var dim_norm__21492 = cljs.core.PersistentVector.fromArray([null, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], true);
  var dim_leap__21493 = cljs.core.PersistentVector.fromArray([null, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], true);
  return function(month, leap_year_QMARK_) {
    return cljs.core._lookup.call(null, cljs.core.truth_(leap_year_QMARK_) ? dim_leap__21493 : dim_norm__21492, month, null)
  }
}();
cljs.reader.parse_and_validate_timestamp = function() {
  var timestamp__21494 = /(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?)?)?)?(?:[Z]|([-+])(\d\d):(\d\d))?/;
  var check__21496 = function(low, n, high, msg) {
    if(function() {
      var and__3822__auto____21495 = low <= n;
      if(and__3822__auto____21495) {
        return n <= high
      }else {
        return and__3822__auto____21495
      }
    }()) {
    }else {
      throw new Error([cljs.core.str("Assert failed: "), cljs.core.str([cljs.core.str(msg), cljs.core.str(" Failed:  "), cljs.core.str(low), cljs.core.str("<="), cljs.core.str(n), cljs.core.str("<="), cljs.core.str(high)].join("")), cljs.core.str("\n"), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'<=", "\ufdd1'low", "\ufdd1'n", "\ufdd1'high"), cljs.core.hash_map("\ufdd0'line", 474))))].join(""));
    }
    return n
  };
  return function(ts) {
    var temp__3974__auto____21497 = cljs.core.map.call(null, cljs.core.vec, cljs.core.split_at.call(null, 8, cljs.core.re_matches.call(null, timestamp__21494, ts)));
    if(cljs.core.truth_(temp__3974__auto____21497)) {
      var vec__21498__21501 = temp__3974__auto____21497;
      var vec__21499__21502 = cljs.core.nth.call(null, vec__21498__21501, 0, null);
      var ___21503 = cljs.core.nth.call(null, vec__21499__21502, 0, null);
      var years__21504 = cljs.core.nth.call(null, vec__21499__21502, 1, null);
      var months__21505 = cljs.core.nth.call(null, vec__21499__21502, 2, null);
      var days__21506 = cljs.core.nth.call(null, vec__21499__21502, 3, null);
      var hours__21507 = cljs.core.nth.call(null, vec__21499__21502, 4, null);
      var minutes__21508 = cljs.core.nth.call(null, vec__21499__21502, 5, null);
      var seconds__21509 = cljs.core.nth.call(null, vec__21499__21502, 6, null);
      var milliseconds__21510 = cljs.core.nth.call(null, vec__21499__21502, 7, null);
      var vec__21500__21511 = cljs.core.nth.call(null, vec__21498__21501, 1, null);
      var ___21512 = cljs.core.nth.call(null, vec__21500__21511, 0, null);
      var ___21513 = cljs.core.nth.call(null, vec__21500__21511, 1, null);
      var ___21514 = cljs.core.nth.call(null, vec__21500__21511, 2, null);
      var V__21515 = vec__21498__21501;
      var vec__21516__21519 = cljs.core.map.call(null, function(v) {
        return cljs.core.map.call(null, function(p1__21491_SHARP_) {
          return parseInt(p1__21491_SHARP_, 10)
        }, v)
      }, cljs.core.map.call(null, function(p1__21489_SHARP_, p2__21488_SHARP_) {
        return cljs.core.update_in.call(null, p2__21488_SHARP_, cljs.core.PersistentVector.fromArray([0], true), p1__21489_SHARP_)
      }, cljs.core.PersistentVector.fromArray([cljs.core.constantly.call(null, null), function(p1__21490_SHARP_) {
        if(cljs.core._EQ_.call(null, p1__21490_SHARP_, "-")) {
          return"-1"
        }else {
          return"1"
        }
      }], true), V__21515));
      var vec__21517__21520 = cljs.core.nth.call(null, vec__21516__21519, 0, null);
      var ___21521 = cljs.core.nth.call(null, vec__21517__21520, 0, null);
      var y__21522 = cljs.core.nth.call(null, vec__21517__21520, 1, null);
      var mo__21523 = cljs.core.nth.call(null, vec__21517__21520, 2, null);
      var d__21524 = cljs.core.nth.call(null, vec__21517__21520, 3, null);
      var h__21525 = cljs.core.nth.call(null, vec__21517__21520, 4, null);
      var m__21526 = cljs.core.nth.call(null, vec__21517__21520, 5, null);
      var s__21527 = cljs.core.nth.call(null, vec__21517__21520, 6, null);
      var ms__21528 = cljs.core.nth.call(null, vec__21517__21520, 7, null);
      var vec__21518__21529 = cljs.core.nth.call(null, vec__21516__21519, 1, null);
      var offset_sign__21530 = cljs.core.nth.call(null, vec__21518__21529, 0, null);
      var offset_hours__21531 = cljs.core.nth.call(null, vec__21518__21529, 1, null);
      var offset_minutes__21532 = cljs.core.nth.call(null, vec__21518__21529, 2, null);
      var offset__21533 = offset_sign__21530 * (offset_hours__21531 * 60 + offset_minutes__21532);
      return cljs.core.PersistentVector.fromArray([cljs.core.not.call(null, years__21504) ? 1970 : y__21522, cljs.core.not.call(null, months__21505) ? 1 : check__21496.call(null, 1, mo__21523, 12, "timestamp month field must be in range 1..12"), cljs.core.not.call(null, days__21506) ? 1 : check__21496.call(null, 1, d__21524, cljs.reader.days_in_month.call(null, mo__21523, cljs.reader.leap_year_QMARK_.call(null, y__21522)), "timestamp day field must be in range 1..last day in month"), cljs.core.not.call(null, 
      hours__21507) ? 0 : check__21496.call(null, 0, h__21525, 23, "timestamp hour field must be in range 0..23"), cljs.core.not.call(null, minutes__21508) ? 0 : check__21496.call(null, 0, m__21526, 59, "timestamp minute field must be in range 0..59"), cljs.core.not.call(null, seconds__21509) ? 0 : check__21496.call(null, 0, s__21527, cljs.core._EQ_.call(null, m__21526, 59) ? 60 : 59, "timestamp second field must be in range 0..60"), cljs.core.not.call(null, milliseconds__21510) ? 0 : check__21496.call(null, 
      0, ms__21528, 999, "timestamp millisecond field must be in range 0..999"), offset__21533], true)
    }else {
      return null
    }
  }
}();
cljs.reader.parse_timestamp = function parse_timestamp(ts) {
  var temp__3971__auto____21545 = cljs.reader.parse_and_validate_timestamp.call(null, ts);
  if(cljs.core.truth_(temp__3971__auto____21545)) {
    var vec__21546__21547 = temp__3971__auto____21545;
    var years__21548 = cljs.core.nth.call(null, vec__21546__21547, 0, null);
    var months__21549 = cljs.core.nth.call(null, vec__21546__21547, 1, null);
    var days__21550 = cljs.core.nth.call(null, vec__21546__21547, 2, null);
    var hours__21551 = cljs.core.nth.call(null, vec__21546__21547, 3, null);
    var minutes__21552 = cljs.core.nth.call(null, vec__21546__21547, 4, null);
    var seconds__21553 = cljs.core.nth.call(null, vec__21546__21547, 5, null);
    var ms__21554 = cljs.core.nth.call(null, vec__21546__21547, 6, null);
    var offset__21555 = cljs.core.nth.call(null, vec__21546__21547, 7, null);
    return new Date(Date.UTC(years__21548, months__21549 - 1, days__21550, hours__21551, minutes__21552, seconds__21553, ms__21554) - offset__21555 * 60 * 1E3)
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
  var tag__21559 = cljs.reader.read_symbol.call(null, rdr, initch);
  var temp__3971__auto____21560 = cljs.core._lookup.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_), cljs.core.name.call(null, tag__21559), null);
  if(cljs.core.truth_(temp__3971__auto____21560)) {
    var pfn__21561 = temp__3971__auto____21560;
    return pfn__21561.call(null, cljs.reader.read.call(null, rdr, true, null, false))
  }else {
    return cljs.reader.reader_error.call(null, rdr, "Could not find tag parser for ", cljs.core.name.call(null, tag__21559), " in ", cljs.core.pr_str.call(null, cljs.core.keys.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_))))
  }
};
cljs.reader.register_tag_parser_BANG_ = function register_tag_parser_BANG_(tag, f) {
  var tag__21564 = cljs.core.name.call(null, tag);
  var old_parser__21565 = cljs.core._lookup.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_), tag__21564, null);
  cljs.core.swap_BANG_.call(null, cljs.reader._STAR_tag_table_STAR_, cljs.core.assoc, tag__21564, f);
  return old_parser__21565
};
cljs.reader.deregister_tag_parser_BANG_ = function deregister_tag_parser_BANG_(tag) {
  var tag__21568 = cljs.core.name.call(null, tag);
  var old_parser__21569 = cljs.core._lookup.call(null, cljs.core.deref.call(null, cljs.reader._STAR_tag_table_STAR_), tag__21568, null);
  cljs.core.swap_BANG_.call(null, cljs.reader._STAR_tag_table_STAR_, cljs.core.dissoc, tag__21568);
  return old_parser__21569
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
      var temp__3971__auto____21117 = jayq.core.crate_meta.call(null, sel);
      if(cljs.core.truth_(temp__3971__auto____21117)) {
        var cm__21118 = temp__3971__auto____21117;
        return[cljs.core.str("[crateGroup="), cljs.core.str(cm__21118), cljs.core.str("]")].join("")
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
  var $__delegate = function(sel, p__21119) {
    var vec__21123__21124 = p__21119;
    var context__21125 = cljs.core.nth.call(null, vec__21123__21124, 0, null);
    if(cljs.core.not.call(null, context__21125)) {
      return jQuery(jayq.core.__GT_selector.call(null, sel))
    }else {
      return jQuery(jayq.core.__GT_selector.call(null, sel), context__21125)
    }
  };
  var $ = function(sel, var_args) {
    var p__21119 = null;
    if(goog.isDef(var_args)) {
      p__21119 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return $__delegate.call(this, sel, p__21119)
  };
  $.cljs$lang$maxFixedArity = 1;
  $.cljs$lang$applyTo = function(arglist__21126) {
    var sel = cljs.core.first(arglist__21126);
    var p__21119 = cljs.core.rest(arglist__21126);
    return $__delegate(sel, p__21119)
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
  var or__3824__auto____21127 = this$.slice(k, k + 1);
  if(cljs.core.truth_(or__3824__auto____21127)) {
    return or__3824__auto____21127
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
  var G__21128 = null;
  var G__21128__2 = function(_, k) {
    return cljs.core._lookup.call(null, this, k)
  };
  var G__21128__3 = function(_, k, not_found) {
    return cljs.core._lookup.call(null, this, k, not_found)
  };
  G__21128 = function(_, k, not_found) {
    switch(arguments.length) {
      case 2:
        return G__21128__2.call(this, _, k);
      case 3:
        return G__21128__3.call(this, _, k, not_found)
    }
    throw"Invalid arity: " + arguments.length;
  };
  return G__21128
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
  var attr__delegate = function($elem, a, p__21129) {
    var vec__21134__21135 = p__21129;
    var v__21136 = cljs.core.nth.call(null, vec__21134__21135, 0, null);
    var a__21137 = cljs.core.name.call(null, a);
    if(cljs.core.not.call(null, v__21136)) {
      return $elem.attr(a__21137)
    }else {
      return $elem.attr(a__21137, v__21136)
    }
  };
  var attr = function($elem, a, var_args) {
    var p__21129 = null;
    if(goog.isDef(var_args)) {
      p__21129 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return attr__delegate.call(this, $elem, a, p__21129)
  };
  attr.cljs$lang$maxFixedArity = 2;
  attr.cljs$lang$applyTo = function(arglist__21138) {
    var $elem = cljs.core.first(arglist__21138);
    var a = cljs.core.first(cljs.core.next(arglist__21138));
    var p__21129 = cljs.core.rest(cljs.core.next(arglist__21138));
    return attr__delegate($elem, a, p__21129)
  };
  attr.cljs$lang$arity$variadic = attr__delegate;
  return attr
}();
jayq.core.remove_attr = function remove_attr($elem, a) {
  return $elem.removeAttr(cljs.core.name.call(null, a))
};
jayq.core.data = function() {
  var data__delegate = function($elem, k, p__21139) {
    var vec__21144__21145 = p__21139;
    var v__21146 = cljs.core.nth.call(null, vec__21144__21145, 0, null);
    var k__21147 = cljs.core.name.call(null, k);
    if(cljs.core.not.call(null, v__21146)) {
      return $elem.data(k__21147)
    }else {
      return $elem.data(k__21147, v__21146)
    }
  };
  var data = function($elem, k, var_args) {
    var p__21139 = null;
    if(goog.isDef(var_args)) {
      p__21139 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return data__delegate.call(this, $elem, k, p__21139)
  };
  data.cljs$lang$maxFixedArity = 2;
  data.cljs$lang$applyTo = function(arglist__21148) {
    var $elem = cljs.core.first(arglist__21148);
    var k = cljs.core.first(cljs.core.next(arglist__21148));
    var p__21139 = cljs.core.rest(cljs.core.next(arglist__21148));
    return data__delegate($elem, k, p__21139)
  };
  data.cljs$lang$arity$variadic = data__delegate;
  return data
}();
jayq.core.position = function position($elem) {
  return cljs.core.js__GT_clj.call(null, $elem.position(), "\ufdd0'keywordize-keys", true)
};
jayq.core.add_class = function add_class($elem, cl) {
  var cl__21150 = cljs.core.name.call(null, cl);
  return $elem.addClass(cl__21150)
};
jayq.core.remove_class = function remove_class($elem, cl) {
  var cl__21152 = cljs.core.name.call(null, cl);
  return $elem.removeClass(cl__21152)
};
jayq.core.toggle_class = function toggle_class($elem, cl) {
  var cl__21154 = cljs.core.name.call(null, cl);
  return $elem.toggleClass(cl__21154)
};
jayq.core.has_class = function has_class($elem, cl) {
  var cl__21156 = cljs.core.name.call(null, cl);
  return $elem.hasClass(cl__21156)
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
  var hide__delegate = function($elem, p__21157) {
    var vec__21162__21163 = p__21157;
    var speed__21164 = cljs.core.nth.call(null, vec__21162__21163, 0, null);
    var on_finish__21165 = cljs.core.nth.call(null, vec__21162__21163, 1, null);
    return $elem.hide(speed__21164, on_finish__21165)
  };
  var hide = function($elem, var_args) {
    var p__21157 = null;
    if(goog.isDef(var_args)) {
      p__21157 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return hide__delegate.call(this, $elem, p__21157)
  };
  hide.cljs$lang$maxFixedArity = 1;
  hide.cljs$lang$applyTo = function(arglist__21166) {
    var $elem = cljs.core.first(arglist__21166);
    var p__21157 = cljs.core.rest(arglist__21166);
    return hide__delegate($elem, p__21157)
  };
  hide.cljs$lang$arity$variadic = hide__delegate;
  return hide
}();
jayq.core.show = function() {
  var show__delegate = function($elem, p__21167) {
    var vec__21172__21173 = p__21167;
    var speed__21174 = cljs.core.nth.call(null, vec__21172__21173, 0, null);
    var on_finish__21175 = cljs.core.nth.call(null, vec__21172__21173, 1, null);
    return $elem.show(speed__21174, on_finish__21175)
  };
  var show = function($elem, var_args) {
    var p__21167 = null;
    if(goog.isDef(var_args)) {
      p__21167 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return show__delegate.call(this, $elem, p__21167)
  };
  show.cljs$lang$maxFixedArity = 1;
  show.cljs$lang$applyTo = function(arglist__21176) {
    var $elem = cljs.core.first(arglist__21176);
    var p__21167 = cljs.core.rest(arglist__21176);
    return show__delegate($elem, p__21167)
  };
  show.cljs$lang$arity$variadic = show__delegate;
  return show
}();
jayq.core.toggle = function() {
  var toggle__delegate = function($elem, p__21177) {
    var vec__21182__21183 = p__21177;
    var speed__21184 = cljs.core.nth.call(null, vec__21182__21183, 0, null);
    var on_finish__21185 = cljs.core.nth.call(null, vec__21182__21183, 1, null);
    return $elem.toggle(speed__21184, on_finish__21185)
  };
  var toggle = function($elem, var_args) {
    var p__21177 = null;
    if(goog.isDef(var_args)) {
      p__21177 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return toggle__delegate.call(this, $elem, p__21177)
  };
  toggle.cljs$lang$maxFixedArity = 1;
  toggle.cljs$lang$applyTo = function(arglist__21186) {
    var $elem = cljs.core.first(arglist__21186);
    var p__21177 = cljs.core.rest(arglist__21186);
    return toggle__delegate($elem, p__21177)
  };
  toggle.cljs$lang$arity$variadic = toggle__delegate;
  return toggle
}();
jayq.core.fade_out = function() {
  var fade_out__delegate = function($elem, p__21187) {
    var vec__21192__21193 = p__21187;
    var speed__21194 = cljs.core.nth.call(null, vec__21192__21193, 0, null);
    var on_finish__21195 = cljs.core.nth.call(null, vec__21192__21193, 1, null);
    return $elem.fadeOut(speed__21194, on_finish__21195)
  };
  var fade_out = function($elem, var_args) {
    var p__21187 = null;
    if(goog.isDef(var_args)) {
      p__21187 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return fade_out__delegate.call(this, $elem, p__21187)
  };
  fade_out.cljs$lang$maxFixedArity = 1;
  fade_out.cljs$lang$applyTo = function(arglist__21196) {
    var $elem = cljs.core.first(arglist__21196);
    var p__21187 = cljs.core.rest(arglist__21196);
    return fade_out__delegate($elem, p__21187)
  };
  fade_out.cljs$lang$arity$variadic = fade_out__delegate;
  return fade_out
}();
jayq.core.fade_in = function() {
  var fade_in__delegate = function($elem, p__21197) {
    var vec__21202__21203 = p__21197;
    var speed__21204 = cljs.core.nth.call(null, vec__21202__21203, 0, null);
    var on_finish__21205 = cljs.core.nth.call(null, vec__21202__21203, 1, null);
    return $elem.fadeIn(speed__21204, on_finish__21205)
  };
  var fade_in = function($elem, var_args) {
    var p__21197 = null;
    if(goog.isDef(var_args)) {
      p__21197 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return fade_in__delegate.call(this, $elem, p__21197)
  };
  fade_in.cljs$lang$maxFixedArity = 1;
  fade_in.cljs$lang$applyTo = function(arglist__21206) {
    var $elem = cljs.core.first(arglist__21206);
    var p__21197 = cljs.core.rest(arglist__21206);
    return fade_in__delegate($elem, p__21197)
  };
  fade_in.cljs$lang$arity$variadic = fade_in__delegate;
  return fade_in
}();
jayq.core.slide_up = function() {
  var slide_up__delegate = function($elem, p__21207) {
    var vec__21212__21213 = p__21207;
    var speed__21214 = cljs.core.nth.call(null, vec__21212__21213, 0, null);
    var on_finish__21215 = cljs.core.nth.call(null, vec__21212__21213, 1, null);
    return $elem.slideUp(speed__21214, on_finish__21215)
  };
  var slide_up = function($elem, var_args) {
    var p__21207 = null;
    if(goog.isDef(var_args)) {
      p__21207 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return slide_up__delegate.call(this, $elem, p__21207)
  };
  slide_up.cljs$lang$maxFixedArity = 1;
  slide_up.cljs$lang$applyTo = function(arglist__21216) {
    var $elem = cljs.core.first(arglist__21216);
    var p__21207 = cljs.core.rest(arglist__21216);
    return slide_up__delegate($elem, p__21207)
  };
  slide_up.cljs$lang$arity$variadic = slide_up__delegate;
  return slide_up
}();
jayq.core.slide_down = function() {
  var slide_down__delegate = function($elem, p__21217) {
    var vec__21222__21223 = p__21217;
    var speed__21224 = cljs.core.nth.call(null, vec__21222__21223, 0, null);
    var on_finish__21225 = cljs.core.nth.call(null, vec__21222__21223, 1, null);
    return $elem.slideDown(speed__21224, on_finish__21225)
  };
  var slide_down = function($elem, var_args) {
    var p__21217 = null;
    if(goog.isDef(var_args)) {
      p__21217 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return slide_down__delegate.call(this, $elem, p__21217)
  };
  slide_down.cljs$lang$maxFixedArity = 1;
  slide_down.cljs$lang$applyTo = function(arglist__21226) {
    var $elem = cljs.core.first(arglist__21226);
    var p__21217 = cljs.core.rest(arglist__21226);
    return slide_down__delegate($elem, p__21217)
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
  var closest__delegate = function($elem, selector, p__21227) {
    var vec__21231__21232 = p__21227;
    var context__21233 = cljs.core.nth.call(null, vec__21231__21232, 0, null);
    return $elem.closest(jayq.core.__GT_selector.call(null, selector), context__21233)
  };
  var closest = function($elem, selector, var_args) {
    var p__21227 = null;
    if(goog.isDef(var_args)) {
      p__21227 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return closest__delegate.call(this, $elem, selector, p__21227)
  };
  closest.cljs$lang$maxFixedArity = 2;
  closest.cljs$lang$applyTo = function(arglist__21234) {
    var $elem = cljs.core.first(arglist__21234);
    var selector = cljs.core.first(cljs.core.next(arglist__21234));
    var p__21227 = cljs.core.rest(cljs.core.next(arglist__21234));
    return closest__delegate($elem, selector, p__21227)
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
  var val__delegate = function($elem, p__21235) {
    var vec__21239__21240 = p__21235;
    var v__21241 = cljs.core.nth.call(null, vec__21239__21240, 0, null);
    if(cljs.core.truth_(v__21241)) {
      return $elem.val(v__21241)
    }else {
      return $elem.val()
    }
  };
  var val = function($elem, var_args) {
    var p__21235 = null;
    if(goog.isDef(var_args)) {
      p__21235 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1), 0)
    }
    return val__delegate.call(this, $elem, p__21235)
  };
  val.cljs$lang$maxFixedArity = 1;
  val.cljs$lang$applyTo = function(arglist__21242) {
    var $elem = cljs.core.first(arglist__21242);
    var p__21235 = cljs.core.rest(arglist__21242);
    return val__delegate($elem, p__21235)
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
jayq.core.xhr = function xhr(p__21243, content, callback) {
  var vec__21249__21250 = p__21243;
  var method__21251 = cljs.core.nth.call(null, vec__21249__21250, 0, null);
  var uri__21252 = cljs.core.nth.call(null, vec__21249__21250, 1, null);
  var params__21253 = jayq.util.clj__GT_js.call(null, cljs.core.ObjMap.fromObject(["\ufdd0'type", "\ufdd0'data", "\ufdd0'success"], {"\ufdd0'type":clojure.string.upper_case.call(null, cljs.core.name.call(null, method__21251)), "\ufdd0'data":jayq.util.clj__GT_js.call(null, content), "\ufdd0'success":callback}));
  return jQuery.ajax(uri__21252, params__21253)
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
  var unbind__delegate = function($elem, ev, p__21254) {
    var vec__21258__21259 = p__21254;
    var func__21260 = cljs.core.nth.call(null, vec__21258__21259, 0, null);
    return $elem.unbind(cljs.core.name.call(null, ev), func__21260)
  };
  var unbind = function($elem, ev, var_args) {
    var p__21254 = null;
    if(goog.isDef(var_args)) {
      p__21254 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return unbind__delegate.call(this, $elem, ev, p__21254)
  };
  unbind.cljs$lang$maxFixedArity = 2;
  unbind.cljs$lang$applyTo = function(arglist__21261) {
    var $elem = cljs.core.first(arglist__21261);
    var ev = cljs.core.first(cljs.core.next(arglist__21261));
    var p__21254 = cljs.core.rest(cljs.core.next(arglist__21261));
    return unbind__delegate($elem, ev, p__21254)
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
  var on__delegate = function($elem, events, p__21262) {
    var vec__21268__21269 = p__21262;
    var sel__21270 = cljs.core.nth.call(null, vec__21268__21269, 0, null);
    var data__21271 = cljs.core.nth.call(null, vec__21268__21269, 1, null);
    var handler__21272 = cljs.core.nth.call(null, vec__21268__21269, 2, null);
    return $elem.on(jayq.core.__GT_event.call(null, events), jayq.core.__GT_selector.call(null, sel__21270), data__21271, handler__21272)
  };
  var on = function($elem, events, var_args) {
    var p__21262 = null;
    if(goog.isDef(var_args)) {
      p__21262 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return on__delegate.call(this, $elem, events, p__21262)
  };
  on.cljs$lang$maxFixedArity = 2;
  on.cljs$lang$applyTo = function(arglist__21273) {
    var $elem = cljs.core.first(arglist__21273);
    var events = cljs.core.first(cljs.core.next(arglist__21273));
    var p__21262 = cljs.core.rest(cljs.core.next(arglist__21273));
    return on__delegate($elem, events, p__21262)
  };
  on.cljs$lang$arity$variadic = on__delegate;
  return on
}();
jayq.core.one = function() {
  var one__delegate = function($elem, events, p__21274) {
    var vec__21280__21281 = p__21274;
    var sel__21282 = cljs.core.nth.call(null, vec__21280__21281, 0, null);
    var data__21283 = cljs.core.nth.call(null, vec__21280__21281, 1, null);
    var handler__21284 = cljs.core.nth.call(null, vec__21280__21281, 2, null);
    return $elem.one(jayq.core.__GT_event.call(null, events), jayq.core.__GT_selector.call(null, sel__21282), data__21283, handler__21284)
  };
  var one = function($elem, events, var_args) {
    var p__21274 = null;
    if(goog.isDef(var_args)) {
      p__21274 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return one__delegate.call(this, $elem, events, p__21274)
  };
  one.cljs$lang$maxFixedArity = 2;
  one.cljs$lang$applyTo = function(arglist__21285) {
    var $elem = cljs.core.first(arglist__21285);
    var events = cljs.core.first(cljs.core.next(arglist__21285));
    var p__21274 = cljs.core.rest(cljs.core.next(arglist__21285));
    return one__delegate($elem, events, p__21274)
  };
  one.cljs$lang$arity$variadic = one__delegate;
  return one
}();
jayq.core.off = function() {
  var off__delegate = function($elem, events, p__21286) {
    var vec__21291__21292 = p__21286;
    var sel__21293 = cljs.core.nth.call(null, vec__21291__21292, 0, null);
    var handler__21294 = cljs.core.nth.call(null, vec__21291__21292, 1, null);
    return $elem.off(jayq.core.__GT_event.call(null, events), jayq.core.__GT_selector.call(null, sel__21293), handler__21294)
  };
  var off = function($elem, events, var_args) {
    var p__21286 = null;
    if(goog.isDef(var_args)) {
      p__21286 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 2), 0)
    }
    return off__delegate.call(this, $elem, events, p__21286)
  };
  off.cljs$lang$maxFixedArity = 2;
  off.cljs$lang$applyTo = function(arglist__21295) {
    var $elem = cljs.core.first(arglist__21295);
    var events = cljs.core.first(cljs.core.next(arglist__21295));
    var p__21286 = cljs.core.rest(cljs.core.next(arglist__21295));
    return off__delegate($elem, events, p__21286)
  };
  off.cljs$lang$arity$variadic = off__delegate;
  return off
}();
jayq.core.prevent = function prevent(e) {
  return e.preventDefault()
};
goog.provide("catb.views.pmt.questions");
goog.require("cljs.core");
goog.require("catb.i18n");
goog.require("jayq.util");
goog.require("catb.views.core");
goog.require("jayq.core");
goog.require("catb.dispatch");
goog.require("catb.backbone.core");
goog.require("clojure.string");
goog.require("catb.views.core");
goog.require("catb.i18n");
goog.require("jayq.core");
goog.require("jayq.util");
catb.views.pmt.questions.current_fact_value = function current_fact_value(question, fact_number) {
  var col__20635 = cljs.core.nth.call(null, (new cljs.core.Keyword("\ufdd0'values")).call(null, question), fact_number, "\ufdd0'catb.views.pmt.questions/not-found");
  var v__20636 = cljs.core.coll_QMARK_.call(null, col__20635) ? cljs.core.first.call(null, col__20635) : null;
  return v__20636
};
catb.views.pmt.questions.get_radio_widget_html = function get_radio_widget_html(question, fact_number) {
  var inputid__20639 = cljs.core.gensym.call(null, cljs.core.format.call(null, "input-q%s-", (new cljs.core.Keyword("\ufdd0'id")).call(null, question)));
  return cljs.core.apply.call(null, cljs.core.str, cljs.core.map.call(null, function(formalanswer, answer) {
    var checked__20640 = cljs.core._EQ_.call(null, catb.views.pmt.questions.current_fact_value.call(null, question, fact_number), formalanswer) ? "checked" : "";
    return cljs.core.format.call(null, '<input class="radiobutton inputfield required" name="%s" value="%s" type="radio" %s/>%s', inputid__20639, formalanswer, checked__20640, answer)
  }, (new cljs.core.Keyword("\ufdd0'formalanswers")).call(null, question), (new cljs.core.Keyword("\ufdd0'answers")).call(null, question)))
};
catb.views.pmt.questions.select_widget = function select_widget(question, fact_number) {
  return[cljs.core.str(cljs.core.format.call(null, '<select type="select" class="combobox required"> ')), cljs.core.str(cljs.core.apply.call(null, cljs.core.str, cljs.core.map.call(null, function(value, name) {
    var selected__20642 = cljs.core._EQ_.call(null, catb.views.pmt.questions.current_fact_value.call(null, question, fact_number), value) ? "selected" : "";
    return cljs.core.format.call(null, '<option class="dropdown-menu inputfield" value="%s" %s>%s</option>', value, selected__20642, name)
  }, (new cljs.core.Keyword("\ufdd0'type")).call(null, question), (new cljs.core.Keyword("\ufdd0'typename")).call(null, question)))), cljs.core.str("</select>")].join("")
};
catb.views.pmt.questions.input_widget = function input_widget(question, fact_number) {
  var value__20646 = function() {
    var or__3824__auto____20645 = catb.views.pmt.questions.current_fact_value.call(null, question, fact_number);
    if(cljs.core.truth_(or__3824__auto____20645)) {
      return or__3824__auto____20645
    }else {
      return""
    }
  }();
  return cljs.core.format.call(null, '<input class="inputfield required" type="text">%s</input>', value__20646)
};
catb.views.pmt.questions.radio_widget = function radio_widget(values, names) {
  var inputname__20648 = cljs.core.gensym.call(null, "name");
  return cljs.core.apply.call(null, cljs.core.str, cljs.core.map.call(null, function(value, name) {
    return cljs.core.format.call(null, '<input class="radiobutton inputfield required" name="%s" value="%s" type="radio"/>%s  ', inputname__20648, value, name)
  }, values, names))
};
catb.views.pmt.questions.replace_variables_by_widgets = function replace_variables_by_widgets(text, widgets) {
  var wid__20651 = cljs.core.atom.call(null, widgets);
  return clojure.string.replace.call(null, text, /\?\w+/, function() {
    var G__20653__delegate = function(_) {
      var w__20652 = cljs.core.first.call(null, cljs.core.deref.call(null, wid__20651));
      cljs.core.swap_BANG_.call(null, wid__20651, cljs.core.next);
      return w__20652
    };
    var G__20653 = function(var_args) {
      var _ = null;
      if(goog.isDef(var_args)) {
        _ = cljs.core.array_seq(Array.prototype.slice.call(arguments, 0), 0)
      }
      return G__20653__delegate.call(this, _)
    };
    G__20653.cljs$lang$maxFixedArity = 0;
    G__20653.cljs$lang$applyTo = function(arglist__20654) {
      var _ = cljs.core.seq(arglist__20654);
      return G__20653__delegate(_)
    };
    G__20653.cljs$lang$arity$variadic = G__20653__delegate;
    return G__20653
  }())
};
catb.views.pmt.questions.functional_QMARK_ = function functional_QMARK_(question) {
  var and__3822__auto____20656 = cljs.core._EQ_.call(null, (new cljs.core.Keyword("\ufdd0'min")).call(null, question), 1);
  if(and__3822__auto____20656) {
    return cljs.core._EQ_.call(null, (new cljs.core.Keyword("\ufdd0'max")).call(null, question), 1)
  }else {
    return and__3822__auto____20656
  }
};
catb.views.pmt.questions.build_facts_buttons = function build_facts_buttons(question) {
  if(function() {
    var and__3822__auto____20659 = cljs.core.not_EQ_.call(null, (new cljs.core.Keyword("\ufdd0'max")).call(null, question), 1);
    if(and__3822__auto____20659) {
      var and__3822__auto____20660 = cljs.core.not.call(null, (new cljs.core.Keyword("\ufdd0'concept")).call(null, question));
      if(and__3822__auto____20660) {
        return cljs.core.not.call(null, (new cljs.core.Keyword("\ufdd0'grounded")).call(null, question))
      }else {
        return and__3822__auto____20660
      }
    }else {
      return and__3822__auto____20659
    }
  }()) {
    return'&nbsp;&nbsp;<img class="remove-fact fact-button" src="images/list-remove.png"/>\n<img class="add-fact fact-button" src="images/list-add.png"/>'
  }else {
    return""
  }
};
catb.views.pmt.questions.widget_for_role = function widget_for_role(question, fact_number) {
  if(cljs.core.coll_QMARK_.call(null, (new cljs.core.Keyword("\ufdd0'type")).call(null, question))) {
    return catb.views.pmt.questions.select_widget.call(null, question, fact_number)
  }else {
    if(function() {
      var or__3824__auto____20662 = cljs.core._EQ_.call(null, (new cljs.core.Keyword("\ufdd0'type")).call(null, question), "symbol");
      if(or__3824__auto____20662) {
        return or__3824__auto____20662
      }else {
        return cljs.core._EQ_.call(null, (new cljs.core.Keyword("\ufdd0'type")).call(null, question), "string")
      }
    }()) {
      return catb.views.pmt.questions.input_widget.call(null, question, fact_number)
    }else {
      if("\ufdd0'else") {
        throw"NYI";
      }else {
        return null
      }
    }
  }
};
catb.views.pmt.questions.create_questions_map = function create_questions_map() {
  return cljs.core.ObjMap.fromObject(["\ufdd0'order", "\ufdd0'questions", "\ufdd0'latest-questions", "\ufdd0'deleted"], {"\ufdd0'order":cljs.core.PersistentVector.EMPTY, "\ufdd0'questions":cljs.core.ObjMap.EMPTY, "\ufdd0'latest-questions":cljs.core.PersistentVector.EMPTY, "\ufdd0'deleted":cljs.core.List.EMPTY})
};
catb.views.pmt.questions.questions = cljs.core.atom.call(null, catb.views.pmt.questions.create_questions_map.call(null));
catb.views.pmt.questions.get_question_el = function get_question_el(question) {
  return jayq.core.$.call(null, [cljs.core.str("#q"), cljs.core.str((new cljs.core.Keyword("\ufdd0'id")).call(null, question))].join(""))
};
catb.views.pmt.questions.add_facts_number_listener = function add_facts_number_listener(question) {
  var el__20669 = catb.views.pmt.questions.get_question_el.call(null, question);
  var G__20670__20671 = cljs.core.seq.call(null, el__20669.find(".add-fact"));
  while(true) {
    if(G__20670__20671) {
      var add_button__20672 = cljs.core.first.call(null, G__20670__20671);
      var add_button__20673 = jayq.core.$.call(null, add_button__20672);
      add_button__20673.off("click");
      add_button__20673.click(function(G__20670__20671, add_button__20673, add_button__20672) {
        return function(_) {
          return catb.dispatch.fire.call(null, "\ufdd0'add-fact", cljs.core.ObjMap.fromObject(["\ufdd0'id"], {"\ufdd0'id":(new cljs.core.Keyword("\ufdd0'id")).call(null, question)}))
        }
      }(G__20670__20671, add_button__20673, add_button__20672));
      var G__20675 = cljs.core.next.call(null, G__20670__20671);
      G__20670__20671 = G__20675;
      continue
    }else {
    }
    break
  }
  return cljs.core.doall.call(null, cljs.core.map.call(null, function(remove_button, idx) {
    var remove_button__20674 = jayq.core.$.call(null, remove_button);
    remove_button__20674.off("click");
    return remove_button__20674.click(function(event) {
      return catb.dispatch.fire.call(null, "\ufdd0'remove-fact", cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'fact-nb", "\ufdd0'event"], {"\ufdd0'id":(new cljs.core.Keyword("\ufdd0'id")).call(null, question), "\ufdd0'fact-nb":idx, "\ufdd0'event":event}))
    })
  }, el__20669.find(".remove-fact"), cljs.core.range.call(null)))
};
catb.views.pmt.questions.coll_fetcher = function coll_fetcher(question) {
  var el__20678 = catb.views.pmt.questions.get_question_el.call(null, question);
  var selects__20679 = el__20678.find("select");
  return cljs.core.map.call(null, function(select) {
    return cljs.core.PersistentVector.fromArray([jayq.core.$.call(null, select).val()], true)
  }, selects__20679)
};
catb.views.pmt.questions.string_fetcher = function string_fetcher(question) {
  var el__20682 = catb.views.pmt.questions.get_question_el.call(null, question);
  var fields__20683 = el__20682.find(".inputfield");
  return cljs.core.map.call(null, function(field) {
    return cljs.core.PersistentVector.fromArray([jayq.core.$.call(null, field).val()], true)
  }, fields__20683)
};
catb.views.pmt.questions.create_role_values_fetcher = function create_role_values_fetcher(question) {
  return function() {
    if(cljs.core.coll_QMARK_.call(null, (new cljs.core.Keyword("\ufdd0'type")).call(null, question))) {
      return catb.views.pmt.questions.coll_fetcher.call(null, question)
    }else {
      if(function() {
        var or__3824__auto____20685 = cljs.core._EQ_.call(null, (new cljs.core.Keyword("\ufdd0'type")).call(null, question), "symbol");
        if(or__3824__auto____20685) {
          return or__3824__auto____20685
        }else {
          return cljs.core._EQ_.call(null, (new cljs.core.Keyword("\ufdd0'type")).call(null, question), "string")
        }
      }()) {
        return catb.views.pmt.questions.string_fetcher.call(null, question)
      }else {
        return null
      }
    }
  }
};
catb.views.pmt.questions.add_fetcher = function add_fetcher(questions, id, fetcher) {
  return cljs.core.assoc_in.call(null, questions, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id, "\ufdd0'fetch-values"], true), fetcher)
};
catb.views.pmt.questions.build_role_question_fact_html = function build_role_question_fact_html(question, idx) {
  var capitalized_text__20689 = clojure.string.capitalize.call(null, (new cljs.core.Keyword("\ufdd0'text")).call(null, question));
  var widget__20690 = catb.views.pmt.questions.widget_for_role.call(null, question, idx);
  return[cljs.core.str("<div>"), cljs.core.str(catb.views.pmt.questions.replace_variables_by_widgets.call(null, capitalized_text__20689, cljs.core.PersistentVector.fromArray([widget__20690], true))), cljs.core.str(catb.views.pmt.questions.build_facts_buttons.call(null, question)), cljs.core.str("</div>")].join("")
};
catb.views.pmt.questions.build_role_question_facts_html = function build_role_question_facts_html(question) {
  return cljs.core.apply.call(null, cljs.core.str, cljs.core.map.call(null, function(p1__20686_SHARP_) {
    return catb.views.pmt.questions.build_role_question_fact_html.call(null, question, p1__20686_SHARP_)
  }, cljs.core.range.call(null, (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, question))))
};
catb.views.pmt.questions.create_role_fact_adder = function create_role_fact_adder(question, el) {
  return function() {
    var id__20694 = (new cljs.core.Keyword("\ufdd0'id")).call(null, question);
    var lquestion__20695 = cljs.core.get_in.call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions), cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20694], true));
    var nb_facts__20696 = (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, lquestion__20695);
    jayq.core.append.call(null, el, catb.views.pmt.questions.build_role_question_fact_html.call(null, question, nb_facts__20696 + 1));
    catb.views.pmt.questions.add_facts_number_listener.call(null, question);
    return cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.update_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20694, "\ufdd0'nb-facts"], true), cljs.core.inc)
  }
};
catb.views.pmt.questions.fact_uuid = function fact_uuid(question, fact_nb) {
  jayq.util.log.call(null, "question =");
  jayq.util.log.call(null, jayq.util.clj__GT_js.call(null, question));
  var and__3822__auto____20698 = fact_nb < cljs.core.count.call(null, (new cljs.core.Keyword("\ufdd0'facts-uuid")).call(null, question));
  if(and__3822__auto____20698) {
    return(new cljs.core.Keyword("\ufdd0'facts-uuid")).call(null, question).call(null, fact_nb)
  }else {
    return and__3822__auto____20698
  }
};
catb.views.pmt.questions.create_fact_remover = function create_fact_remover(question, el) {
  return function(event, idx) {
    var target__20705 = event.target;
    var parent__20706 = jayq.core.$.call(null, target__20705).parent();
    var id__20707 = (new cljs.core.Keyword("\ufdd0'id")).call(null, question);
    var lquestion__20708 = cljs.core.get_in.call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions), cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20707], true));
    parent__20706.remove();
    var temp__3974__auto____20709 = catb.views.pmt.questions.fact_uuid.call(null, lquestion__20708, idx);
    if(cljs.core.truth_(temp__3974__auto____20709)) {
      var uuid__20710 = temp__3974__auto____20709;
      cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.assoc_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", (new cljs.core.Keyword("\ufdd0'id")).call(null, lquestion__20708), "\ufdd0'facts-uuid", idx], true), null);
      cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.update_in, cljs.core.PersistentVector.fromArray(["\ufdd0'deleted"], true), cljs.core.conj, uuid__20710)
    }else {
    }
    return cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.update_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", (new cljs.core.Keyword("\ufdd0'id")).call(null, lquestion__20708), "\ufdd0'nb-facts"], true), cljs.core.dec)
  }
};
catb.views.pmt.questions.build_role_question_html = function build_role_question_html(question) {
  return[cljs.core.str(cljs.core.format.call(null, '<div id="q%s">', (new cljs.core.Keyword("\ufdd0'id")).call(null, question))), cljs.core.str(catb.views.pmt.questions.build_role_question_facts_html.call(null, question)), cljs.core.str("</div>")].join("")
};
catb.views.pmt.questions.build_role_question = function build_role_question(question) {
  if(cljs.core.not.call(null, (new cljs.core.Keyword("\ufdd0'grounded")).call(null, question))) {
  }else {
    throw new Error([cljs.core.str("Assert failed: "), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'not", cljs.core.with_meta(cljs.core.list("\ufdd0'grounded", "\ufdd1'question"), cljs.core.hash_map("\ufdd0'line", 243))), cljs.core.hash_map("\ufdd0'line", 243))))].join(""));
  }
  var el__20712 = jayq.core.$.call(null, catb.views.pmt.questions.build_role_question_html.call(null, question));
  return cljs.core.ObjMap.fromObject(["\ufdd0'el", "\ufdd0'fetcher", "\ufdd0'fact-adder", "\ufdd0'fact-remover"], {"\ufdd0'el":el__20712, "\ufdd0'fetcher":catb.views.pmt.questions.create_role_values_fetcher.call(null, question), "\ufdd0'fact-adder":catb.views.pmt.questions.create_role_fact_adder.call(null, question, el__20712), "\ufdd0'fact-remover":catb.views.pmt.questions.create_fact_remover.call(null, question, el__20712)})
};
catb.views.pmt.questions.build_grounded_question_fact_html = function build_grounded_question_fact_html(question, idx) {
  var text__20715 = clojure.string.capitalize.call(null, (new cljs.core.Keyword("\ufdd0'text")).call(null, question));
  return[cljs.core.str(cljs.core.format.call(null, "<div>%s", text__20715)), cljs.core.str(catb.views.pmt.questions.get_radio_widget_html.call(null, question, idx)), cljs.core.str(catb.views.pmt.questions.build_facts_buttons.call(null, question)), cljs.core.str("</div>")].join("")
};
catb.views.pmt.questions.build_grounded_question_facts_html = function build_grounded_question_facts_html(question) {
  return cljs.core.apply.call(null, cljs.core.str, cljs.core.map.call(null, function(p1__20713_SHARP_) {
    return catb.views.pmt.questions.build_grounded_question_fact_html.call(null, question, p1__20713_SHARP_)
  }, cljs.core.range.call(null, (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, question))))
};
catb.views.pmt.questions.build_grounded_question_html = function build_grounded_question_html(question) {
  return[cljs.core.str(cljs.core.format.call(null, '<div id="q%s">', (new cljs.core.Keyword("\ufdd0'id")).call(null, question))), cljs.core.str(catb.views.pmt.questions.build_grounded_question_facts_html.call(null, question)), cljs.core.str("</div>")].join("")
};
catb.views.pmt.questions.create_grounded_question_fetcher = function create_grounded_question_fetcher(question) {
  return function() {
    var el__20718 = catb.views.pmt.questions.get_question_el.call(null, question);
    var inputs__20719 = el__20718.find("input:checked");
    return cljs.core.map.call(null, function(i) {
      return cljs.core.PersistentVector.fromArray([jayq.core.$.call(null, i).val()], true)
    }, inputs__20719)
  }
};
catb.views.pmt.questions.create_grounded_fact_adder = function create_grounded_fact_adder(question, el) {
  return function() {
    var id__20723 = (new cljs.core.Keyword("\ufdd0'id")).call(null, question);
    var lquestion__20724 = cljs.core.get_in.call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions), cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20723], true));
    var nb_facts__20725 = (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, lquestion__20724);
    jayq.core.append.call(null, el, catb.views.pmt.questions.build_grounded_question_fact_html.call(null, question, nb_facts__20725 + 1));
    catb.views.pmt.questions.add_facts_number_listener.call(null, question);
    return cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.update_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20723, "\ufdd0'nb-facts"], true), cljs.core.inc)
  }
};
catb.views.pmt.questions.build_grounded_question = function build_grounded_question(question) {
  var el__20727 = jayq.core.$.call(null, catb.views.pmt.questions.build_grounded_question_html.call(null, question));
  return cljs.core.ObjMap.fromObject(["\ufdd0'el", "\ufdd0'fetcher", "\ufdd0'fact-adder", "\ufdd0'fact-remover"], {"\ufdd0'el":el__20727, "\ufdd0'fetcher":catb.views.pmt.questions.create_grounded_question_fetcher.call(null, question), "\ufdd0'fact-adder":catb.views.pmt.questions.create_grounded_fact_adder.call(null, question, el__20727), "\ufdd0'fact-remover":catb.views.pmt.questions.create_fact_remover.call(null, question, el__20727)})
};
catb.views.pmt.questions.build_question = function build_question(question) {
  jayq.util.log.call(null, "Question");
  jayq.util.log.call(null, jayq.util.clj__GT_js.call(null, question));
  if(cljs.core.truth_((new cljs.core.Keyword("\ufdd0'grounded")).call(null, question))) {
    return catb.views.pmt.questions.build_grounded_question.call(null, question)
  }else {
    if(cljs.core.truth_((new cljs.core.Keyword("\ufdd0'role")).call(null, question))) {
      return catb.views.pmt.questions.build_role_question.call(null, question)
    }else {
      return null
    }
  }
};
catb.views.pmt.questions.add_fact = function add_fact(msg) {
  var id__20732 = (new cljs.core.Keyword("\ufdd0'id")).call(null, msg);
  var question__20733 = cljs.core.get_in.call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions), cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20732], true));
  var nb_facts__20734 = (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, question__20733);
  if(cljs.core.truth_(function() {
    var and__3822__auto____20735 = (new cljs.core.Keyword("\ufdd0'max")).call(null, question__20733);
    if(cljs.core.truth_(and__3822__auto____20735)) {
      return(new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, question__20733) + 1 > (new cljs.core.Keyword("\ufdd0'max")).call(null, question__20733)
    }else {
      return and__3822__auto____20735
    }
  }())) {
    return PM.on_error(catb.i18n.i18n.call(null, "pmt_maximum_number_of_facts"))
  }else {
    return(new cljs.core.Keyword("\ufdd0'fact-adder")).call(null, question__20733).call(null)
  }
};
catb.dispatch.react_to.call(null, cljs.core.PersistentHashSet.fromArray(["\ufdd0'add-fact"]), function(_, msg) {
  return catb.views.pmt.questions.add_fact.call(null, msg)
});
catb.views.pmt.questions.remove_fact = function remove_fact(msg) {
  var id__20743 = (new cljs.core.Keyword("\ufdd0'id")).call(null, msg);
  var fact_nb__20744 = (new cljs.core.Keyword("\ufdd0'fact-nb")).call(null, msg);
  var question__20745 = cljs.core.get_in.call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions), cljs.core.PersistentVector.fromArray(["\ufdd0'questions", id__20743], true));
  var nb_facts__20746 = (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, question__20745);
  var min__20748 = function() {
    var or__3824__auto____20747 = (new cljs.core.Keyword("\ufdd0'min")).call(null, question__20745);
    if(cljs.core.truth_(or__3824__auto____20747)) {
      return or__3824__auto____20747
    }else {
      return 1
    }
  }();
  if(function() {
    var or__3824__auto____20749 = cljs.core._EQ_.call(null, nb_facts__20746, 1);
    if(or__3824__auto____20749) {
      return or__3824__auto____20749
    }else {
      return nb_facts__20746 <= min__20748
    }
  }()) {
    return PM.on_error(catb.i18n.i18n.call(null, "pmt_cannot_remove_fact"))
  }else {
    return(new cljs.core.Keyword("\ufdd0'fact-remover")).call(null, question__20745).call(null, (new cljs.core.Keyword("\ufdd0'event")).call(null, msg), (new cljs.core.Keyword("\ufdd0'fact-nb")).call(null, msg))
  }
};
catb.dispatch.react_to.call(null, cljs.core.PersistentHashSet.fromArray(["\ufdd0'remove-fact"]), function(_, msg) {
  return catb.views.pmt.questions.remove_fact.call(null, msg)
});
catb.views.pmt.questions.add_question_html = function add_question_html(question, el, e) {
  jayq.core.append.call(null, el, cljs.core.format.call(null, "<p><i>%s</i></p>", function() {
    var or__3824__auto____20751 = (new cljs.core.Keyword("\ufdd0'hint")).call(null, question);
    if(cljs.core.truth_(or__3824__auto____20751)) {
      return or__3824__auto____20751
    }else {
      return""
    }
  }()));
  jayq.core.append.call(null, el, e);
  return jayq.core.append.call(null, el, "<br/>")
};
catb.views.pmt.questions.add_question = function add_question(question, htmlel) {
  var map__20759__20760 = catb.views.pmt.questions.build_question.call(null, question);
  var map__20759__20761 = cljs.core.seq_QMARK_.call(null, map__20759__20760) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20759__20760) : map__20759__20760;
  var fetcher__20762 = cljs.core._lookup.call(null, map__20759__20761, "\ufdd0'fetcher", null);
  var fact_remover__20763 = cljs.core._lookup.call(null, map__20759__20761, "\ufdd0'fact-remover", null);
  var fact_adder__20764 = cljs.core._lookup.call(null, map__20759__20761, "\ufdd0'fact-adder", null);
  var el__20765 = cljs.core._lookup.call(null, map__20759__20761, "\ufdd0'el", null);
  catb.views.pmt.questions.add_question_html.call(null, question, htmlel, el__20765);
  catb.views.pmt.questions.add_facts_number_listener.call(null, question);
  cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, catb.views.pmt.questions.add_fetcher, (new cljs.core.Keyword("\ufdd0'id")).call(null, question), fetcher__20762);
  cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.assoc_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", (new cljs.core.Keyword("\ufdd0'id")).call(null, question), "\ufdd0'fact-adder"], true), fact_adder__20764);
  return cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.assoc_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", (new cljs.core.Keyword("\ufdd0'id")).call(null, question), "\ufdd0'fact-remover"], true), fact_remover__20763)
};
catb.views.pmt.questions.add_questions = function add_questions(questions, el) {
  var G__20769__20770 = cljs.core.seq.call(null, questions);
  while(true) {
    if(G__20769__20770) {
      var question__20771 = cljs.core.first.call(null, G__20769__20770);
      catb.views.pmt.questions.add_question.call(null, question__20771, el);
      var G__20772 = cljs.core.next.call(null, G__20769__20770);
      G__20769__20770 = G__20772;
      continue
    }else {
      return null
    }
    break
  }
};
catb.views.pmt.questions.add_submit_button = function add_submit_button(questions_el) {
  var button_id__20774 = [cljs.core.str(cljs.core.gensym.call(null, "button"))].join("");
  jayq.core.append.call(null, questions_el, cljs.core.format.call(null, '<input type="button" value="%s" id="%s"/> ', catb.i18n.i18n.call(null, "pmt_submit"), button_id__20774));
  jayq.core.append.call(null, questions_el, "<hr/>");
  return jayq.core.$.call(null, [cljs.core.str("#"), cljs.core.str(button_id__20774)].join("")).click(function(_) {
    return catb.dispatch.fire.call(null, "\ufdd0'on-submit", cljs.core.ObjMap.EMPTY)
  })
};
catb.views.pmt.questions.display_questions_in_category = function display_questions_in_category(questions, el) {
  var category_name__20776 = (new cljs.core.Keyword("\ufdd0'category_name")).call(null, cljs.core.first.call(null, questions));
  jayq.core.append.call(null, el, cljs.core.format.call(null, "<h3>%s</h3>", category_name__20776));
  return catb.views.pmt.questions.add_questions.call(null, questions, el)
};
catb.views.pmt.questions.display_questions = function display_questions(_, msg) {
  jayq.util.log.call(null, "Display question");
  var el__20783 = jayq.core.$.call(null, "#questions");
  var questions_to_ids__20784 = (new cljs.core.Keyword("\ufdd0'questions")).call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions));
  var ids__20785 = (new cljs.core.Keyword("\ufdd0'order")).call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions));
  jayq.core.empty.call(null, el__20783);
  var G__20786__20787 = cljs.core.seq.call(null, ids__20785);
  while(true) {
    if(G__20786__20787) {
      var ids_for_category__20788 = cljs.core.first.call(null, G__20786__20787);
      catb.views.pmt.questions.display_questions_in_category.call(null, cljs.core.map.call(null, questions_to_ids__20784, ids_for_category__20788), el__20783);
      var G__20789 = cljs.core.next.call(null, G__20786__20787);
      G__20786__20787 = G__20789;
      continue
    }else {
    }
    break
  }
  catb.views.pmt.questions.add_submit_button.call(null, el__20783);
  return PM.scroll_to_bottom()
};
catb.dispatch.react_to.call(null, cljs.core.PersistentHashSet.fromArray(["\ufdd0'questions-added"]), catb.views.pmt.questions.display_questions);
catb.views.pmt.questions.calc_nb_facts = function calc_nb_facts(question) {
  var or__3824__auto____20792 = (new cljs.core.Keyword("\ufdd0'nb-facts")).call(null, question);
  if(cljs.core.truth_(or__3824__auto____20792)) {
    return or__3824__auto____20792
  }else {
    if(function() {
      var or__3824__auto____20793 = (new cljs.core.Keyword("\ufdd0'min")).call(null, question) == null;
      if(or__3824__auto____20793) {
        return or__3824__auto____20793
      }else {
        return(new cljs.core.Keyword("\ufdd0'min")).call(null, question) === 0
      }
    }()) {
      return 1
    }else {
      return(new cljs.core.Keyword("\ufdd0'min")).call(null, question)
    }
  }
};
catb.views.pmt.questions.questions_list__GT_map = function questions_list__GT_map(questions_list) {
  return cljs.core.reduce.call(null, function(m, question) {
    var nb_facts__20797 = catb.views.pmt.questions.calc_nb_facts.call(null, question);
    var question__20798 = cljs.core.assoc.call(null, question, "\ufdd0'nb-facts", nb_facts__20797);
    return cljs.core.assoc.call(null, m, (new cljs.core.Keyword("\ufdd0'id")).call(null, question__20798), question__20798)
  }, cljs.core.ObjMap.EMPTY, questions_list)
};
catb.views.pmt.questions.questions_ordered = function questions_ordered(questions_list) {
  if(cljs.core.map_QMARK_.call(null, cljs.core.first.call(null, questions_list))) {
  }else {
    throw new Error([cljs.core.str("Assert failed: "), cljs.core.str(cljs.core.pr_str.call(null, cljs.core.with_meta(cljs.core.list("\ufdd1'map?", cljs.core.with_meta(cljs.core.list("\ufdd1'first", "\ufdd1'questions-list"), cljs.core.hash_map("\ufdd0'line", 422))), cljs.core.hash_map("\ufdd0'line", 422))))].join(""));
  }
  return cljs.core.map.call(null, function(p1__20794_SHARP_) {
    return cljs.core.map.call(null, "\ufdd0'id", p1__20794_SHARP_)
  }, cljs.core.vals.call(null, cljs.core.group_by.call(null, "\ufdd0'category", questions_list)))
};
catb.views.pmt.questions.index_questions = function index_questions(questions, latest_questions_list) {
  return cljs.core.update_in.call(null, cljs.core.assoc.call(null, cljs.core.update_in.call(null, questions, cljs.core.PersistentVector.fromArray(["\ufdd0'questions"], true), cljs.core.merge, catb.views.pmt.questions.questions_list__GT_map.call(null, latest_questions_list)), "\ufdd0'latest-questions", cljs.core.map.call(null, "\ufdd0'id", latest_questions_list)), cljs.core.PersistentVector.fromArray(["\ufdd0'order"], true), cljs.core.concat, catb.views.pmt.questions.questions_ordered.call(null, latest_questions_list))
};
catb.views.pmt.questions.fetch_values = function fetch_values(qid) {
  var values__20800 = cljs.core.get_in.call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions), cljs.core.PersistentVector.fromArray(["\ufdd0'questions", qid, "\ufdd0'fetch-values"], true)).call(null);
  cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.assoc_in, cljs.core.PersistentVector.fromArray(["\ufdd0'questions", qid, "\ufdd0'values"], true), values__20800);
  return values__20800
};
catb.views.pmt.questions.fetch_latest_questions_answers = function fetch_latest_questions_answers() {
  var latest__20803 = (new cljs.core.Keyword("\ufdd0'latest-questions")).call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions));
  var answers__20804 = cljs.core.map.call(null, catb.views.pmt.questions.fetch_values, latest__20803);
  return cljs.core.map.call(null, function(id, ans) {
    return cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'values"], {"\ufdd0'id":id, "\ufdd0'values":ans})
  }, latest__20803, answers__20804)
};
catb.views.pmt.questions.send_answers = function send_answers(msg) {
  if(cljs.core.truth_(jayq.core.$.call(null, "#questionsform").valid())) {
    var answers__20806 = catb.views.pmt.questions.fetch_latest_questions_answers.call(null);
    jayq.util.log.call(null, "sending answers...");
    jayq.util.log.call(null, jayq.util.clj__GT_js.call(null, answers__20806));
    PM.busy_cursor_on();
    return PM.ajax_post(IMPACT.simulation_url, jayq.util.clj__GT_js.call(null, cljs.core.ObjMap.fromObject(["\ufdd0'answers"], {"\ufdd0'answers":answers__20806})), function(data) {
      PM.busy_cursor_off();
      return catb.views.pmt.questions.show_questions_or_ag.call(null, data)
    }, IMPACT.user, IMPACT.password, PM.on_error)
  }else {
    return null
  }
};
catb.dispatch.react_to.call(null, cljs.core.PersistentHashSet.fromArray(["\ufdd0'on-submit"]), function(msg) {
  return(new cljs.core.Keyword("\ufdd0'submit-listener")).call(null, cljs.core.deref.call(null, catb.views.pmt.questions.questions)).call(null, msg)
});
catb.views.pmt.questions.send_answers_for_modification = function send_answers_for_modification() {
  if(cljs.core.truth_(jayq.core.$.call(null, "#questionsform").valid())) {
    var answers__20813 = catb.views.pmt.questions.fetch_latest_questions_answers.call(null);
    jayq.util.log.call(null, "sending answers for modification...");
    jayq.util.log.call(null, jayq.util.clj__GT_js.call(null, answers__20813));
    PM.busy_cursor_on();
    var map__20814__20815 = cljs.core.deref.call(null, catb.views.pmt.questions.questions);
    var map__20814__20816 = cljs.core.seq_QMARK_.call(null, map__20814__20815) ? cljs.core.apply.call(null, cljs.core.hash_map, map__20814__20815) : map__20814__20815;
    var deleted__20817 = cljs.core._lookup.call(null, map__20814__20816, "\ufdd0'deleted", null);
    var questions__20818 = cljs.core._lookup.call(null, map__20814__20816, "\ufdd0'questions", null);
    return PM.ajax_post(IMPACT.simulation_url, jayq.util.clj__GT_js.call(null, cljs.core.ObjMap.fromObject(["\ufdd0'modify-facts"], {"\ufdd0'modify-facts":cljs.core.ObjMap.fromObject(["\ufdd0'facts", "\ufdd0'deleted", "\ufdd0'db"], {"\ufdd0'facts":cljs.core.vals.call(null, questions__20818), "\ufdd0'deleted":deleted__20817, "\ufdd0'db":IMPACT.db})})), function(data) {
      PM.busy_cursor_off();
      return catb.views.pmt.questions.show_ag.call(null, data.db)
    }, IMPACT.user, IMPACT.password, PM.on_error)
  }else {
    return null
  }
};
catb.views.pmt.questions.show_questions = function show_questions(latest_questions_list) {
  var latest__20820 = cljs.core.js__GT_clj.call(null, latest_questions_list, "\ufdd0'keywordize-keys", true);
  cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, catb.views.pmt.questions.index_questions, latest__20820);
  return catb.dispatch.fire.call(null, "\ufdd0'questions-added", cljs.core.ObjMap.fromObject(["\ufdd0'latest-questions"], {"\ufdd0'latest-questions":latest__20820}))
};
goog.exportSymbol("catb.views.pmt.questions.show_questions", catb.views.pmt.questions.show_questions);
catb.views.pmt.questions.show_ag = function show_ag(db) {
  IMPACT.db = db;
  cljs.core.reset_BANG_.call(null, catb.views.pmt.questions.questions, catb.views.pmt.questions.create_questions_map.call(null));
  return PM.set_arguments_url(db)
};
catb.views.pmt.questions.show_facts = function show_facts(questions_list) {
  cljs.core.reset_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.assoc.call(null, catb.views.pmt.questions.create_questions_map.call(null), "\ufdd0'submit-listener", catb.views.pmt.questions.send_answers_for_modification));
  return catb.views.pmt.questions.show_questions.call(null, questions_list)
};
catb.views.pmt.questions.show_questions_or_ag = function show_questions_or_ag(data) {
  var temp__3971__auto____20823 = data.questions;
  if(cljs.core.truth_(temp__3971__auto____20823)) {
    var questions_list__20824 = temp__3971__auto____20823;
    return catb.views.pmt.questions.show_questions.call(null, questions_list__20824)
  }else {
    return catb.views.pmt.questions.show_ag.call(null, data.db)
  }
};
catb.views.pmt.questions.init_show_questions = function init_show_questions() {
  cljs.core.swap_BANG_.call(null, catb.views.pmt.questions.questions, cljs.core.assoc, "\ufdd0'submit-listener", catb.views.pmt.questions.send_answers);
  return PM.ajax_post(IMPACT.simulation_url, jayq.util.clj__GT_js.call(null, cljs.core.ObjMap.fromObject(["\ufdd0'request"], {"\ufdd0'request":IMPACT.question})), catb.views.pmt.questions.show_questions_or_ag, IMPACT.user, IMPACT.password, PM.on_error)
};
goog.exportSymbol("catb.views.pmt.questions.init_show_questions", catb.views.pmt.questions.init_show_questions);
goog.provide("catb.test.questions");
goog.require("cljs.core");
goog.require("jayq.util");
goog.require("jayq.core");
goog.require("catb.views.pmt.questions");
goog.require("catb.views.pmt.questions");
goog.require("jayq.util");
goog.require("jayq.core");
catb.test.questions.q_role_yn = cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'category_name", "\ufdd0'hint", "\ufdd0'text", "\ufdd0'role", "\ufdd0'yesnoquestion"], {"\ufdd0'id":"\ufdd1'q-role-yn", "\ufdd0'category_name":"License (Role Y/N)", "\ufdd0'hint":"License information.", "\ufdd0'text":"Does X has a license to do Y?", "\ufdd0'role":true, "\ufdd0'yesnoquestion":true});
catb.test.questions.q_role = cljs.core.ObjMap.fromObject(["\ufdd0'category_name", "\ufdd0'text", "\ufdd0'typename", "\ufdd0'max", "\ufdd0'type", "\ufdd0'hint", "\ufdd0'min", "\ufdd0'role", "\ufdd0'yesnoquestion", "\ufdd0'id"], {"\ufdd0'category_name":"Usage (Role)", "\ufdd0'text":"Usage X for ?Y", "\ufdd0'typename":cljs.core.PersistentVector.fromArray(["commercial", "non-commercial"], true), "\ufdd0'max":1, "\ufdd0'type":cljs.core.vec(["\ufdd1'commercial", "\ufdd1'non-commercial"]), "\ufdd0'hint":"Usage information.", 
"\ufdd0'min":1, "\ufdd0'role":true, "\ufdd0'yesnoquestion":false, "\ufdd0'id":"\ufdd1'q-role"});
catb.test.questions.q_concept = cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'category_name", "\ufdd0'hint", "\ufdd0'text", "\ufdd0'concept", "\ufdd0'min", "\ufdd0'max", "\ufdd0'yesnoquestion"], {"\ufdd0'id":"\ufdd1'q-concept", "\ufdd0'category_name":"Father (Concept Y/N)", "\ufdd0'hint":"Father information.", "\ufdd0'text":"Is X the father of Y?", "\ufdd0'concept":true, "\ufdd0'min":1, "\ufdd0'max":1, "\ufdd0'yesnoquestion":true});
catb.test.questions.q_predicate_yn = cljs.core.ObjMap.fromObject(["\ufdd0'category_name", "\ufdd0'text", "\ufdd0'max", "\ufdd0'hint", "\ufdd0'min", "\ufdd0'yesnoquestion", "\ufdd0'answers", "\ufdd0'id", "\ufdd0'formalanswers"], {"\ufdd0'category_name":"Type of use (Predicate Y/N)", "\ufdd0'text":"a uses b for commercial purposes", "\ufdd0'max":1, "\ufdd0'hint":"Type of use information.", "\ufdd0'min":1, "\ufdd0'yesnoquestion":true, "\ufdd0'answers":cljs.core.PersistentVector.fromArray(["Yes", "No", 
"Maybe"], true), "\ufdd0'id":"\ufdd1'q-predicate-yn", "\ufdd0'formalanswers":cljs.core.PersistentVector.fromArray(["yes", "no", "maybe"], true)});
catb.test.questions.q_predicate = cljs.core.ObjMap.fromObject(["\ufdd0'id", "\ufdd0'category_name", "\ufdd0'hint", "\ufdd0'text", "\ufdd0'widgets", "\ufdd0'min", "\ufdd0'max", "\ufdd0'yesnoquestion"], {"\ufdd0'id":"\ufdd1'q-predicate", "\ufdd0'category_name":"Type of use (Predicate)", "\ufdd0'hint":"Type of use information.", "\ufdd0'text":"?a uses ?b for ?p purposes", "\ufdd0'widgets":cljs.core.PersistentVector.fromArray(["text", "text", "text"], true), "\ufdd0'min":1, "\ufdd0'max":1, "\ufdd0'yesnoquestion":false});
catb.test.questions.questions = cljs.core.PersistentVector.fromArray([catb.test.questions.q_role_yn, catb.test.questions.q_role, catb.test.questions.q_concept, catb.test.questions.q_predicate_yn, catb.test.questions.q_predicate], true);
catb.test.questions.prepare = function prepare() {
  var G__16702__16703 = cljs.core.seq.call(null, catb.test.questions.questions);
  while(true) {
    if(G__16702__16703) {
      var q__16704 = cljs.core.first.call(null, G__16702__16703);
      catb.views.pmt.questions.show_questions.call(null, jayq.util.clj__GT_js.call(null, cljs.core.PersistentVector.fromArray([q__16704], true)), jayq.core.$.call(null, "#questions"), function(G__16702__16703, q__16704) {
        return function(_) {
          return alert("on submit")
        }
      }(G__16702__16703, q__16704));
      var G__16705 = cljs.core.next.call(null, G__16702__16703);
      G__16702__16703 = G__16705;
      continue
    }else {
      return null
    }
    break
  }
};
goog.exportSymbol("catb.test.questions.prepare", catb.test.questions.prepare);
catb.test.questions.run = function run() {
  return true
};
goog.exportSymbol("catb.test.questions.run", catb.test.questions.run);
goog.provide("catb.test");
goog.require("cljs.core");
goog.require("catb.test.navigation");
catb.test.success = 0;
catb.test.run = function run() {
  console.log("Example test started.");
  navigation.run.call(null);
  return catb.test.success
};
goog.exportSymbol("catb.test.run", catb.test.run);
