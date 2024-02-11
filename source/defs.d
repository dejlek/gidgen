module defs;

import std.utf : toUTF32;

import import_symbols;
import gir.func;
import gir.param;
import gir.repo;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;
import xml_patch;
import xml_tree;

enum DefsCmdPrefix = "//!"d; /// Prefix used for definition commands
enum DefsCommentPrefix = "//#"d; /// Prefix used for definition comments (not included in content)

class Defs
{
  this()
  {
  }

  /**
   * Load definition control files from a directory.
   * Params:
   *   path = Path of definition files
   */
  void loadDefFiles(string path = "defs")
  {
    string[] classFiles;

    foreach (string filename; dirEntries(path, "*.d", SpanMode.shallow))
    { // Process class files, which contain a dash in the filename, after the main repo files
      if (filename.baseName.canFind('-'))
        classFiles ~= filename;
      else
        loadDefs(filename);
    }

    foreach (filename; classFiles)
      loadDefs(filename);
  }

  /**
   * Load a definition file. Filenames are of the form "Namespace.d" and "Namespace-Class.d",
   * such as "GLib.d" and "GLib-Boxed.d".
   *
   * Params:
   *   filename = The name of the definition file to load
   */
  void loadDefs(string filename)
  { // Curly brace block argument processing state machine
    enum BlockState
    {
      None, // Not processing a block
      Start, // Expecting opening brace '{'
      Content, // Processing content until '}'
    }

    auto fileData = readText(filename).to!dstring; // The definition file data converted to a unicode dstring
    dstring[] cmdTokens; // Parsed definition command tokens (space separated and quoted strings)
    uint lineCount; // Current line count
    BlockState block; // Multi-line block processing state
    Repo curRepo; // Current repo or null
    dstring curStructName; // Current structure name or null
    bool inClass; // true if currently inside class (a raw code declaration or "class" command)

    string posInfo()
    {
      return "(file '" ~ filename ~ "' line " ~ lineCount.to!string ~ ")";
    }

    // Process raw code lines in definition files (not definition commands)
    void processDefCode(dstring line)
    {
      auto defCode = curRepo.structDefCode[curStructName];

      if (line.startsWith("import ")) // Import statement?
      {
        try
          defCode.imports.parseImport(line);
        catch(Exception e)
          stderr.writeln(e.msg, " ", posInfo);

        return;
      }

      if (!inClass) // Not inside class?
      { // Is this a class declaration?
        if (line.startsWith("class") || line.startsWith("abstract class"))
        {
          defCode.classDecl = line;
          inClass = true;
        }
        else
          defCode.preClass ~= line; // Append to pre class content
      }
      else
        defCode.inClass ~= line; // Append to inside class content
    }

    auto classSplitName = filename.baseName.stripExtension.split('-');
    auto repoName = classSplitName[0];

    if (classSplitName.length > 1) // Is this a repo class file? Will have a dash separator between the namespace and class names.
    {
      auto findRepo = repos.find!(x => x.namespace.to!string == repoName);

      if (findRepo.length > 0) // Is there a repo with the matching namespace from the filename?
      { // Set current repo and struct name from filename
        curRepo = findRepo[0];
        curStructName = classSplitName[1].to!dstring;

        if (curStructName !in curRepo.structDefCode) // If structure definition code wasn't already created, create it
          curRepo.structDefCode[curStructName] = new DefCode();
      }
      else
      {
        stderr.writeln("Ignoring class definition file '", filename, "' with no matching repository '",
          classSplitName[0], "'");
        return;
      }
    }

    foreach (line; fileData.splitLines) // Loop on definition file lines
    {
      lineCount++;
      auto lineStrip = line.strip;

      if (lineStrip.startsWith(DefsCommentPrefix)) // Skip definition comments
        continue;

      dstring cmdLine;

      if (lineStrip.startsWith(DefsCmdPrefix))
        cmdLine = lineStrip[DefsCmdPrefix.length .. $];

      if (!cmdLine)
      { // All lines which aren't definition commands get processed as potential code
        if (!curStructName.empty)
          processDefCode(lineStrip);

        continue;
      }

      if (block == BlockState.Start) // Expecting block start?
      {
        if (cmdLine.startsWith('{'))
        {
          block = BlockState.Content;
          continue;
        }

        stderr.writeln("'", cmdTokens[0], "' command requires ", cmdTokens.length, " arguments ", posInfo);
        block = BlockState.None; // Just ignore previous command and continue to process the line
      }

      if (block == BlockState.None)
      {
        if (cmdLine.empty) // Skip empty command lines
          continue;

        cmdTokens = cmdLine.splitQuoted.array; // Parse command tokens
      }
      else if (block == BlockState.Content) // Processing multi-line brace block content
      {
        if (!cmdLine.startsWith('}'))
        { // Append content to last command argument and advance to next line
          cmdTokens[$ - 1] ~= cmdLine ~ "\n";
          continue;
        }

        block = BlockState.None; // Closing brace of multi-line block argument, fall through to process the command
      }

      auto cmd = cmdTokens[0];
      auto findCmdInfo = defCommandInfo.find!(x => x.name == cmd);
      if (findCmdInfo.empty)
      {
        stderr.writeln("Unknown command '", cmd, "'");
        continue;
      }

      auto cmdInfo = findCmdInfo[0];

      if ((cmdInfo.flags & DefCmdFlags.AllowBlock) && cmdTokens.length == cmdInfo.argCount) // Allow multi-line block arguments for some commands
      {
        block = BlockState.Start;
        cmdTokens ~= "";
        continue;
      }

      if (cmdTokens.length != cmdInfo.argCount + 1)
      {
        stderr.writeln("'", cmd, "' command requires ", cmdInfo.argCount, " ", cmdInfo.argCount == 1
          ? "argument " : "arguments ", posInfo);
        break;
      }

      if (cmdInfo.flags & DefCmdFlags.ReqRepo && !curRepo)
      {
        stderr.writeln("'", cmd, "' command requires 'repo' to be specified");
        continue;
      }

      if (cmdInfo.flags & DefCmdFlags.ReqClass && !curStructName)
      {
        stderr.writeln("'", cmd, "' command requires 'struct' to be specified");
        continue;
      }

      switch (cmd)
      {
        case "add", "del", "rename", "set":
          auto patch = new XmlPatch();

          try
          {
            final switch (cmd)
            {
              case "add":
                patch.parseAddCmd(cmdTokens[1], cmdTokens[2]);
                break;
              case "del":
                patch.parseDeleteCmd(cmdTokens[1]);
                break;
              case "rename":
                patch.parseRenameCmd(cmdTokens[1], cmdTokens[2]);
                break;
              case "set":
                patch.parseSetCmd(cmdTokens[1], cmdTokens[2]);
                break;
            }
          }
          catch (XmlPatchError e)
          {
            stderr.writeln("XML patch error: ", e.msg, " ", posInfo);
            break;
          }

          if (curRepo)
            curRepo.patches ~= patch;
          else
            patches ~= patch;
          break;
        case "class":
          curStructName = cmdTokens[1];

          if (curStructName !in curRepo.structDefCode)
            curRepo.structDefCode[curStructName] = new DefCode();
          else
            stderr.writeln("Duplicate class command found for '", curStructName, "' ", posInfo);
          break;
        case "import":
          curRepo.structDefCode[curStructName].imports.add(cmdTokens[1]);
          break;
        case "repo":
          curRepo = new Repo(this, cmdTokens[1].to!string);
          curRepo.namespace = repoName.to!dstring;
          repos ~= curRepo;
          curStructName = null;
          break;
        case "reserved":
          reservedWords[cmdTokens[1]] = true;
          break;
        case "subtype":
          typeof(typeSubs)* subs = curRepo ? &curRepo.typeSubs : &typeSubs;

          if (cmdTokens[1] !in *subs)
            (*subs)[cmdTokens[1]] = cmdTokens[2];
          else
            stderr.writeln("subtype '", cmdTokens[1], "' already exists ", posInfo);
          break;
        default:
          assert(0);
      }
    }
  }

  /**
   * Load gir files specified in the loaded definition files to Repo objects.
   * Params:
   *   girPaths = Array of paths to search for Gir files
   */
  void loadRepos(string[] girPaths = ["/usr/share/gir-1.0"])
  {
    foreach (repo; repos)
    {
      string filePath;
      foreach (search; girPaths) // Search for Gir file in search paths
      {
        auto p = buildPath(search, repo.filename ~ ".gir");
        if (isFile(p))
        {
          filePath = p;
          break;
        }
      }

      if (filePath.empty)
        throw new Exception("Repository Gir file '" ~ repo.filename ~ "' not found (search paths = "
          ~ girPaths.join(":") ~ ")");

      repo.filename = filePath;
      auto tree = new XmlTree();
      tree.parse(readText(repo.filename).toUTF32, repo.filename); // Load the Gir XML file

      XmlNode namespaceNode;

      if (tree.root)
      {
        foreach (n; tree.root.children)
          if (n.id == "namespace")
            namespaceNode = n;
      }

      if (!namespaceNode)
        throw new Exception("No 'namespace' XML node found in Gir file");

      foreach (patch; patches) // Apply global XML patches from defs file
      {
        try
          patch.apply(tree, namespaceNode);
        catch (XmlPatchError e)
        {}
      }

      foreach (patch; repo.patches) // Apply XML patches from defs file
        patch.apply(tree, namespaceNode);

      repo.fromXmlTree(tree); // Convert XML tree to Gir object tree
    }

    fixupRepos();
  }

  /// Ensure consistent state of repo data and fixup additional data (array parameter indexes, etc)
  private void fixupRepos()
  {
    foreach (repo; repos)
    {
      repoHash[repo.namespace] = repo;

      foreach (al; repo.aliases)
      {
        try
          al.fixup;
        catch (Exception e)
        {
          al.disable = true;
          stderr.writeln("Disabling alias '" ~ al.fullName.to!string ~ "': " ~ e.msg);
        }
      }

      foreach (con; repo.constants)
      {
        try
          con.fixup;
        catch (Exception e)
        {
          con.disable = true;
          stderr.writeln("Disabling constant '" ~ con.fullName.to!string ~ "': " ~ e.msg);
        }
      }

      foreach (st; repo.structs)
      {
        foreach (fn; st.functions)
          fn.fixup;

        if (st.kind == TypeKind.Wrap)
        {
          if (!st.freeFunction)
            stderr.writeln("Wrapped structure " ~ st.fullName.to!string ~ " has no free-function");

          foreach (f; st.fields)
          {
            try
              f.fixup;
            catch (Exception e)
            {
              f.disable = true;
              stderr.writeln("Disabling field '" ~ f.fullName.to!string ~ "': " ~ e.msg);
              continue;
            }

            if (f.kind.among(TypeKind.Unknown, TypeKind.Opaque, TypeKind.Interface, TypeKind.Namespace))
            {
              f.disable = true;
              stderr.writeln("Disabling field " ~ f.fullName.to!string ~ " with unhandled type '"
                ~ f.subDType.to!string ~ "' (" ~ f.kind.to!string ~ ")");
            }
            else if (f.writable && f.kind.among(TypeKind.Boxed, TypeKind.Wrap, TypeKind.Reffed))
            {
              f.writable = false;
              stderr.writeln("Setting writable to false for field " ~ f.fullName.to!string ~ " with unhandled type '"
                ~ f.subDType.to!string ~ "' (" ~ f.kind.to!string ~ ")");
            }
          }

          foreach (prop; st.properties)
          {
            try
              prop.fixup;
            catch (Exception e)
            {
              prop.disable = true;
              stderr.writeln("Disabling property '" ~ prop.fullName.to!string ~ "': " ~ e.msg);
              continue;
            }

            if (prop.kind.among(TypeKind.Unknown, TypeKind.Opaque, TypeKind.Interface, TypeKind.Namespace))
            {
              prop.disable = true;
              stderr.writeln("Disabling property " ~ prop.fullName.to!string ~ " with unhandled type '"
                ~ prop.subDType.to!string ~ "' (" ~ prop.kind.to!string ~ ")");
            }
            else if (prop.writable && prop.kind.among(TypeKind.Boxed, TypeKind.Wrap, TypeKind.Reffed))
            {
              prop.writable = false;
              stderr.writeln("Setting writable to false for property " ~ prop.fullName.to!string
                ~ " with unhandled type '" ~ prop.subDType.to!string ~ "' (" ~ prop.kind.to!string ~ ")");
            }
          }
        }
      }

      foreach (dcArray; repo.structDefCode.byKeyValue) // Loop on structure definitions and assign to structs
      {
        auto st = repo.structHash.get(dcArray.key, null);

        if (!st) // Create new class structures if non-existant
        {
          st = new Structure(repo);
          st.name = dcArray.key;
          st.structType = StructType.Class;
          repo.addStruct(st);
        }

        st.defCode = dcArray.value;
      }

      repo.structs.sort!((x, y) => x.name < y.name); // Sort structures by name
    }
  }

  /**
   * Write the packages for the loaded Repo objects defined in the definitions files.
   * Params:
   *   basePath = The path to the toplevel packages directory (defaults to "packages")
   *   packagePrefix = Prefix to add to package name (defaults to "gid-")
   */
  void writePackages(string basePath = "packages")
  {
    foreach (repo; repos)
      repo.writePackage(basePath);
  }

  /**
   * Get the kind classification of a type string
   * Params:
   *   type = The type string (a C or D type)
   *   repo = The repo the type was found in
   * Returns: The type classification, falls back to TypeKind.Basic if no other type applies
   */
  TypeKind typeKind(dstring type, Repo repo)
  {
    if (type.among("char*"d, "const char*"d, "string"d, "utf8"d))
      return TypeKind.String;

    auto ndx = type.countUntil('.');

    if (ndx != -1)
    {
      if (auto pRepo = type in repoHash)
        repo = *pRepo;

      type = type[ndx + 1 .. $];
    }

    if (auto en = type in repo.enumHash)
      return en.bitfield ? TypeKind.Flags : TypeKind.Enum;

    if (auto st = type in repo.structHash)
      return st.kind;

    return TypeKind.Basic;
  }

  /**
   * Find type node object by D type name which may include the namespace separated by a period.
   * Params:
   *   typeName = Type name string
   *   namespace = Default namespace or null
   * Returns: The matching type node or null if not found (possible basic type)
   */
  TypeNode findTypeNode(dstring typeName, dstring namespace)
  {
    auto t = typeName.split('.');
    if (t.length > 1)
    {
      namespace = t[0];
      typeName = t[1];
    }

    if (auto repo = namespace in repoHash)
    {
      if (auto en = typeName in repo.enumHash)
        return cast(TypeNode)en;

      if (auto st = typeName in repo.structHash)
        return cast(TypeNode)st;
    }

    return null;
  }

  /**
   * Derive an array member C type from an array C type.
   * Params:
   *   arrayCType = The C type string of the array
   * Returns: The member C type or null if unable to determine
   */
  dstring getArrayMemberCType(dstring arrayCType)
  {
    auto isConst = arrayCType.startsWith("const");

    if (isConst) // Strip const and () parenthesis from const(TYPE*)* expressions
      arrayCType = arrayCType[5 .. $].filter!(x => x != '(' && x != ')').array;

    auto starCount = arrayCType.retro.countUntil!(x => x != '*');

    if (starCount == -1)
      return null;

    arrayCType = arrayCType[0 .. $ - starCount]; // Strip the stars off

    if (isConst && starCount > 1)
      return "const(" ~ arrayCType ~ "*"d.replicate(starCount - 2) ~ ")*";

    return arrayCType ~ "*"d.replicate(starCount - 1);
  }

  /**
   * Fix symbol name if it is a reserved word, by appending an underscore to it.
   * Params:
   *   name = The symbol name
   * Returns: The symbol name possibly with underscore appended if it is a reserved Dlang word
   */
  dstring symbolName(dstring name)
  {
    if (name in reservedWords) // Add underscore to reserved words
      return name ~ "_";
    else if (!name.empty && name[0] >= '0' && name[0] <= '9') // Starts with a digit? Can happen with some shortened enum members.
      return "_" ~ name;

    return name;
  }

  /**
   * Substitute type.
   * Params:
   *   type = Type string
   *   localSubs = Local substitution map or null (default)
   * Returns: Type string with any relevant substitutions
   */
  dstring subTypeStr(dstring type, dstring[dstring] localSubs = null)
  {
    auto tok = tokenizeType(type).filter!(x => x != "volatile").array;

    if (tok.empty)
      return type;

    // Substitute types not including end '*' pointers
    dstring sub(dstring s)
    {
      for (auto i = cast(int)(s.length) - 1; i >= 0; i--)
      {
        if (s[i] != '*' && s[i] != ' ')
        {
          auto t = s[0 .. i + 1];

          if (t in localSubs)
            t = localSubs[t];
          else
            t = typeSubs.get(t, t);

          return t ~ s[i + 1 .. $];
        }
      }

      return s;
    }

    // Remove multiple "const", format as const(TYPE)*, and substitute type names
    if (tok[0] == "const" && tok[$ - 1] == "*")
      return "const(" ~ sub(tok[0 .. $ - 1].filter!(x => !x.among("const"d, "("d, ")"d)).join("")) ~ ")*";

    return sub(tok.filter!(x => x != "const")
      .map!(x => x == "*" ? x : " " ~ x)
      .join("").strip);
  }

  bool[dstring] reservedWords; /// Reserved words (_ appended)
  dstring[dstring] typeSubs; /// Global type substitutions
  XmlPatch[] patches; /// Global XML patches specified in definitions file
  Repo[] repos; /// Gir repositories

  Repo[dstring] repoHash; /// Hash of repositories by namespace
}

/// Kind of a type
enum TypeKind
{
  Unknown, /// Unknown type
  Basic, /// A basic data type
  String, /// A string
  Enum, /// Enumeration type
  Flags, /// Bitfield flags type
  Simple, /// Simple Record or Union with basic fields (Basic, Enum, Flags) and no methods (alias to C type)
  Opaque, /// Opaque Record pointer type with no accessible fields or methods (alias to C type)
  Wrap, /// Record or Union with accessible fields and/or methods (wrap with a class)
  Boxed, /// A GLib boxed Record type
  Reffed, /// Referenced Class type with inheritence (not GObject derived)
  Object, /// A GObject Class
  Interface, /// Interface type
  Namespace, /// Namespace structure (no C type, global module for example)
}

/**
 * Check if a type kind has a module file.
 * Params:
 *   kind = The type kind
 * Returns: true if the given type kind is implemented with its own module
 */
bool typeKindHasModule(TypeKind kind)
{
  return kind >= TypeKind.Wrap;
}

/**
 * Check if a type kind is defined in a global module.
 * Params:
 *   kind = The type kind
 * Returns: true if the given type kind is defined in a global module
 */
bool typeKindIsGlobal(TypeKind kind)
{
  return kind >= TypeKind.Enum && kind <= TypeKind.Opaque;
}

/// Manual code from a definitions file
class DefCode
{
  this()
  {
    imports = new ImportSymbols();
  }

  ImportSymbols imports; /// Imports
  dstring[] preClass; /// Pre class declaration code, line separated
  dstring classDecl; /// Class declaration
  dstring[] inClass; /// Code inside of the class, line separated
}

/// Definition command flags
enum DefCmdFlags
{
  AllowBlock = 1 << 0, /// Allow a multi-line block argument (last arg)
  ReqRepo = 1 << 1, /// Require repo to have been specified
  ReqClass = 1 << 2, /// Require struct to have been specified
  None = 0,
}

/// Definition command info
struct DefCmd
{
  dstring name;
  int argCount;
  BitFlags!DefCmdFlags flags;
}

// Command information
immutable DefCmd[] defCommandInfo = [
  {"add", 2, DefCmdFlags.AllowBlock},
  {"class", 1, DefCmdFlags.ReqRepo},
  {"del", 1, DefCmdFlags.None},
  {"import", 1, DefCmdFlags.ReqClass},
  {"rename", 2, DefCmdFlags.None},
  {"repo", 1, DefCmdFlags.None},
  {"reserved", 1, DefCmdFlags.None},
  {"set", 2, DefCmdFlags.AllowBlock},
  {"subtype", 2, DefCmdFlags.None},
];
