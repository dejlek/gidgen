module gir.structure;

import code_writer;
import defs;
import import_symbols;
import gir.base;
import gir.field;
import gir.func;
import gir.func_writer;
import gir.property;
import gir.repo;
import gir.type_node;
import utils;

/// Structure class which is used for class, interface, and records in Gir files
final class Structure : TypeNode
{
  this(Base parent)
  {
    super(parent);
    defCode = new DefCode;
  }

  this(Base parent, XmlNode node)
  {
    this(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return dType;
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    structType = cast(StructType)StructTypeValues.countUntil(node.id);
    cSymbolPrefix = node.get("c:symbol-prefix");
    parentType = node.get("parent");
    version_ = node.get("version");

    abstract_ = node.get("abstract") == "1";
    deprecated_ = node.get("deprecated") == "1";
    opaque = node.get("opaque") == "1";
    glibFundamental = node.get("glib:fundamental") == "1";

    deprecatedVersion = node.get("deprecated-version");
    glibGetType = node.get("glib:get-type");
    glibTypeName = node.get("glib:type-name");
    glibGetValueFunc = node.get("glib:get-value-func");
    glibSetValueFunc = node.get("glib:set-value-func");
    glibRefFunc = node.get("glib:ref-func");
    glibUnrefFunc = node.get("glib:unref-func");
    glibTypeStruct = node.get("glib:type-struct");
    glibIsGtypeStructFor = node.get("glib:is-gtype-struct-for");
    copyFunction = node.get("copy-function");
    freeFunction = node.get("free-function");
  }

  private TypeKind calcKind()
  {
    if (cType.empty)
      return TypeKind.Namespace;

    if (structType == StructType.Record && !glibGetType.empty)
      return TypeKind.Boxed;

    if (structType == StructType.Record && opaque && functions.empty)
      return TypeKind.Opaque;

    if (structType == StructType.Record || structType == StructType.Union)
    {
      if (!functions.empty)
        return TypeKind.Wrap;

      foreach (field; fields)
        if (!field.callback && !field.kind.among(TypeKind.Basic, TypeKind.Enum, TypeKind.Flags))
          return TypeKind.Wrap;

      return TypeKind.Simple;
    }

    if (structType == StructType.Class && glibFundamental
        && (!parentType.empty || (!glibRefFunc.empty && !glibUnrefFunc.empty)))
      return TypeKind.Reffed;

    if (structType == StructType.Class && !parentType.empty && !glibGetType.empty)
      return TypeKind.Object;

    if (structType == StructType.Interface)
      return TypeKind.Interface;

    if (dType == "ObjectG") // HACK: ObjectG is the OG Object
      return TypeKind.Object;

    return TypeKind.Unknown;
  }

  // Fixup base type only, not dependencies
  void fixupType()
  {
    super.fixup;
    kind = calcKind;

    if (kind == TypeKind.Boxed && parentType.empty)
      parentType = "GLib.Boxed";
  }

  // Fixup structure dependencies (separated so that all base types are resolved first with fixupType())
  void fixupDeps()
  {
    if (!parentType.empty)
      parentStruct = cast(Structure)repo.defs.findTypeObject(parentType, repo);

    foreach (fn; functions) // Fixup structure function/methods
      fn.fixup;

    foreach (sg; signals) // Fixup structure signals
      sg.fixup;

    foreach (f; fields) // Fixup structure fields
    {
      f.fixup;

      if (opaque)
        f.disable = true;
    }

    foreach (prop; properties) // Fixup structure properties
      prop.fixup;
  }

  override void verify()
  {
    if (disable) // Don't verify if structure is disabled
      return;

    super.verify;

    if (!parentType.empty && !parentStruct)
      throw new Exception("Failed to resolve parent type '" ~ parentType.to!string ~ "'");

    foreach (fn; functions) // Verify structure function/methods
    {
      FuncType[] allowedTypes = [FuncType.Callback, FuncType.Function, FuncType.Constructor, FuncType.Signal,
        FuncType.Method];

      if (!allowedTypes.canFind(fn.funcType))
      {
        fn.disable = true;
        warning("Disabling function " ~ fn.fullName.to!string ~ " of type '" ~ fn.funcType.to!string
          ~ "' which is not supported");
      }
      else
        fn.verify;
    }

    if (kind == TypeKind.Wrap && !freeFunction)
      warning("Wrapped structure " ~ fullName.to!string ~ " has no free-function");

    foreach (f; fields) // Verify structure fields
    {
      try
        f.verify;
      catch (Exception e)
      {
        f.disable = true;
        warning("Disabling field '" ~ f.fullName.to!string ~ "': " ~ e.msg);
        continue;
      }
    }

    foreach (prop; properties) // Verify structure properties
    {
      try
        prop.verify;
      catch (Exception e)
      {
        prop.disable = true;
        warning("Disabling property '" ~ prop.fullName.to!string ~ "': " ~ e.msg);
        continue;
      }

      if (prop.kind.among(TypeKind.Unknown, TypeKind.Callback, TypeKind.Opaque, TypeKind.Interface, TypeKind.Namespace))
      {
        prop.disable = true;
        warning("Disabling property " ~ prop.fullName.to!string ~ " with unhandled type '"
          ~ prop.dType.to!string ~ "' (" ~ prop.kind.to!string ~ ")");
      }
      else if (prop.writable && prop.kind.among(TypeKind.Boxed, TypeKind.Wrap, TypeKind.Reffed))
      {
        prop.writable = false;
        warning("Setting writable to false for property " ~ prop.fullName.to!string
          ~ " with unhandled type '" ~ prop.dType.to!string ~ "' (" ~ prop.kind.to!string ~ ")");
      }
    }
  }

  /**
   * Write structure module.
   * Params:
   *   path = Path to the file to write to
   */
  void write(string path)
  {
    auto writer = new CodeWriter(path);
    writer ~= ["module " ~ fullName ~ ";", ""];

    // Create the function writers first to construct the imports
    auto imports = new ImportSymbols(defCode.imports, repo.namespace);

    imports.add("gid");
    imports.add(repo.namespace ~ ".c.functions");
    imports.add(repo.namespace ~ ".c.types");

    FuncWriter[] funcWriters;
    dstring[] propMethods;

    if (defCode.genFuncs)
    {
      foreach (fn; functions)
      {
        if (!fn.disable)
        {
          funcWriters ~= new FuncWriter(fn);
          imports.merge(funcWriters[$ - 1].imports);
        }
      }

      if (kind == TypeKind.Wrap || kind == TypeKind.Boxed)
        propMethods = constructPropMethods(imports); // Construct wrapper property methods in order to collect imports
    }

    if (parentStruct)
      imports.add(parentStruct.fullDType);

    imports.remove(fullDType);

    if (imports.write(writer))
      writer ~= "";

    if (defCode.preClass.length > 0)
      writer ~= defCode.preClass;

    writeDocs(writer);

    if (defCode.classDecl.empty)
      writer ~= "class " ~ dType ~ (parentStruct ? " : " ~ parentStruct.dType : "");
    else
      writer ~= defCode.classDecl;

    writer ~= "{";

    if (defCode.genInit)
      writeInitCode(writer, propMethods);

    if (defCode.inClass.length > 0)
      writer ~= defCode.inClass;

    if (defCode.genFuncs)
    {
      foreach (fnWriter; funcWriters)
      {
        writer ~= "";
        fnWriter.write(writer);
      }
    }

    writer ~= "}";

    if (defCode.postClass.length > 0)
      writer ~= defCode.postClass;

    writer.write();
  }

  // Write class init code
  private void writeInitCode(CodeWriter writer, dstring[] propMethods)
  {
    if (kind == TypeKind.Wrap) // Wrap a structure in a class with properties
    {
      writer ~= [cType ~ "* cPtr;", ""];
      writer ~= ["this(" ~ cType ~ "* cPtr)", "{",
        "if (!cPtr)", "throw new GidConstructException(\"Null struct pointer for " ~ fullName ~ "\");", "",
        "this.cPtr = cPtr;", "}"];

      if (freeFunction)
        writer ~= ["", "~this()", "{", freeFunction ~ "(cPtr);", "}"];

      writer ~= propMethods;
    }
    else if (kind == TypeKind.Boxed)
    {
      writer ~= ["this(" ~ cType ~ "* cPtr, bool ownedRef = false)", "{",
        "super(cast(void*)cPtr, ownedRef);", "}", ""];
      writer ~= ["auto cPtr()", "{", "return cast(" ~ cType ~ "*)boxPtr;", "}", ""];
      writer ~= ["override GType getType()", "{", "return " ~ glibGetType ~ "();", "}"];

      writer ~= propMethods;
    }
    else if (kind == TypeKind.Reffed)
    {
      if (!parentStruct) // Root fundamental reffed type?
      {
        writer ~= [cType ~ "* cPtr;", ""];
        writer ~= ["this(" ~ cType ~ "* cPtr, bool ownedRef = false)", "{", "this.cPtr = cPtr;", "",
          "if (!ownedRef)", glibRefFunc ~ "(cPtr);", "}", ""];
        writer ~= ["~this()", "{", glibUnrefFunc ~ "(cPtr);", "}", ""];
      }
      else // Derived reffed type
        writer ~= ["this(" ~ cType ~ "* cPtr, bool ownedRef = false)", "{",
          "super(cast(" ~ parentStruct.cType ~ "*)cPtr, ownedRef);", "}", ""];
    }
    else if (kind == TypeKind.Object)
    {
      writer ~= ["this(void* cObj, bool ownedRef = false)", "{", "super(cObj, ownedRef);", "}", ""];
      writer ~= ["T* cPtr(T)()", "if (is(T : " ~ cTypeRemPtr ~ "))", "{", "return cast(T*)objPtr;", "}", ""];
      writer ~= ["static GType getType()", "{", "return " ~ glibGetType ~ "();", "}"];
    }
  }

  // Construct struct wrapper property methods
  private dstring[] constructPropMethods(ImportSymbols imports)
  {
    dstring[] lines;

    foreach(f; fields)
    {
      if (f.disable)
        continue;

      f.addImports(imports, repo);

      auto fieldKind = f.kind;

      lines ~= ["", "@property " ~ f.dType ~ " " ~ f.dName ~ "()", "{"];

      final switch(fieldKind) with(TypeKind)
      {
        case Basic, BasicAlias:
          lines ~= "return cPtr." ~ f.dName ~ ";";
          break;
        case String:
          lines ~= "return cPtr." ~ f.dName ~ ".fromCString(false);";
          break;
        case Enum:
          lines ~= "return cast(" ~ f.dType ~ ")cPtr." ~ f.dName ~ ";";
          break;
        case Flags:
          lines ~= "return " ~ f.dType ~ "(cPtr." ~ f.dName ~ ");";
          break;
        case Simple:
          lines ~= "return *cPtr." ~ f.dName ~ ";";
          break;
        case Boxed:
        case Wrap:
        case Reffed:
          auto starCount = f.cType.retro.countUntil!(x => x != '*');

          if (starCount < 1) // The cast is for casting away "const"
            lines ~= "return new " ~ f.dType ~ "(cast(" ~ f.cType.stripConst ~ "*)" ~ "&cPtr." ~ f.dName ~ ");";
          else
            lines ~= "return new " ~ f.dType ~ "(cast(" ~ f.cType.stripConst ~ ")" ~ "cPtr." ~ f.dName ~ ");";
          break;
        case Object:
          lines ~= "return ObjectG.getDObject!" ~ f.dType ~ "(cPtr!" ~ f.cTypeRemPtr ~ "." ~ f.dName ~ ", false);";
          break;
        case Unknown, Callback, Opaque, Interface, Namespace:
          throw new Exception("Unhandled readable field property type '" ~ f.dType.to!string ~ "' (" ~ fieldKind.to!string
            ~ ") for struct " ~ dType.to!string);
      }

      lines ~= "}";

      if (!f.writable)
        continue;

      lines ~= ["", "@property void " ~ f.dName ~ "(" ~ f.dType ~ " propval)", "{"];

      final switch(fieldKind) with(TypeKind)
      {
        case Basic, BasicAlias:
          lines ~= "cPtr." ~ f.dName ~ " = propval;";
          break;
        case String:
          lines ~= ["g_free(cast(void*)cPtr." ~ f.dName ~ ");", "cPtr." ~ f.dName ~ " = propval.toCString(true);"];
          imports.add("GLib.c.functions");
          break;
        case Enum:
          lines ~= "cPtr." ~ f.dName ~ " = cast(" ~ f.cType ~ ")propval;";
          break;
        case Flags:
          lines ~= "cPtr." ~ f.dName ~ " = cast(" ~ f.cType ~ ")cast(uint)propval;";
          break;
        case Simple:
          lines ~= "cPtr." ~ f.dName ~ " = *propval;";
          break;
        case Object:
          lines ~= "cPtr!" ~ f.cTypeRemPtr ~ "." ~ f.dName ~ " = propval.get" ~ f.dType ~ ";";
          break;
        case Boxed, Callback, Wrap, Reffed, Unknown, Opaque, Interface, Namespace:
          throw new Exception("Unhandled writable field property type '" ~ f.dType.to!string ~ "' (" ~ fieldKind.to!string
            ~ ") for struct " ~ dType.to!string);
      }

      lines ~= "}";
    }

    return lines;
  }

  StructType structType; /// Type of structure
  dstring cSymbolPrefix; /// C symbol prefix
  dstring parentType; /// Parent type name (for derived types)
  Structure parentStruct; /// Resolved parent type object

  dstring[] implements; /// Interfaces implemented by structure
  dstring[] prerequisites; /// Interface prerequisite types
  Func[] functions; /// Constructors, functions, methods, and virtual methods
  Func[] signals; /// Signals
  Field[] fields; /// Structure fields
  Property[] properties; /// Properties

  DefCode defCode; /// Code from definitions file

  bool abstract_; /// Is abstract type?
  bool deprecated_; /// Deprecated?
  bool opaque; /// Opaque structure type
  dstring version_; /// Version
  dstring deprecatedVersion; /// Deprecated version

  bool glibFundamental;
  dstring glibGetType; /// GLib get_type function
  dstring glibTypeName; /// GLib type name
  dstring glibGetValueFunc; /// GLib get value function
  dstring glibSetValueFunc; /// GLib set value function
  dstring glibRefFunc; /// GLib ref function
  dstring glibUnrefFunc; /// GLib unref function
  dstring glibTypeStruct; /// GLib class structure
  dstring glibIsGtypeStructFor; /// Indicates what type a class structure belongs to
  dstring copyFunction; /// Record/Union copy function (not seen in the wild, but defined in gir-1.2.rnc - we use it via XML patching)
  dstring freeFunction; /// Record/Union free function (not seen in the wild, but defined in gir-1.2.rnc - we use it via XML patching)
}

/// Type of structure
enum StructType
{
  Class, /// A class
  Interface, /// An interface
  Record, /// A structure record
  Union, /// A union
}

immutable dstring[] StructTypeValues = [ "class", "interface", "record", "union" ];
