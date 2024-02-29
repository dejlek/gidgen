module gir.param;

import defs;
import gir.func;
import gir.structure;
import gir.type_node;
import std_includes;
import utils;

/// Function parameter
final class Param : TypeNode
{
  this(Base parent, XmlNode node)
  {
    super(parent);
    fromXml(node);
  }

  override @property dstring name()
  {
    return _name;
  }

  override @property void name(dstring val)
  {
    _name = val;
  }

  /// Get the parameter name formatted in D camelCase
  dstring dName()
  {
    return repo.defs.symbolName(name.camelCase);
  }

  /// Get direction string (empty for input, "out ", or "inout "). Intended for use directly in code generation.
  dstring directionStr()
  {
    final switch (direction) with(ParamDirection)
    {
      case In:
        return "";
      case Out:
        return "out ";
      case InOut:
        return "inout ";
    }
  }

  override void fromXml(XmlNode node)
  {
    super.fromXml(node);

    _name = node.get("name");
    isInstanceParam = node.id == "instance-parameter";
    direction = cast(ParamDirection)ParamDirectionValues.countUntil(node.get("direction"));
    ownership = cast(Ownership)OwnershipValues.countUntil(node.get("transfer-ownership"));
    nullable = node.get("nullable") == "1";
    optional = node.get("optional") == "1";
    allowNone = node.get("allow-none") == "1";
    callerAllocates = node.get("caller-allocates") == "1";
    varargs = node.findChild("varargs") !is null;

    if (auto pVal = "closure" in node.attrs)
      closureIndex = (*pVal).to!int;
    else
      closureIndex = NoClosure;

    if (auto pVal = "destroy" in node.attrs)
      destroyIndex = (*pVal).to!int;
    else
      destroyIndex = NoDestroy;

    scope_ = cast(ParamScope)ParamScopeValues.countUntil(node.get("scope"));
  }

  override void fixup()
  {
    if (origDType == "...") // Skip variable args elipsis
      return;

    super.fixup; // Fixup the type node state

    if (lengthParamIndex != ArrayNoLengthParam) // Array has a length argument?
    {
      auto typeFunc = getParentByType!Func;

      if (typeFunc.hasInstanceParam) // Array length parameter indexes don't count instance parameters
        lengthParamIndex++;

      if (lengthParamIndex < typeFunc.params.length)
      {
        auto lengthParam = typeFunc.params[lengthParamIndex];
        lengthParam.arrayParamIndex = cast(int)typeFunc.params.countUntil!(x => x is this);
        lengthParam.isArrayLength = true;
      }
    }
  }

  override void verify()
  {
    super.verify;

    if (containerType == ContainerType.HashTable)
    {
      if (direction != ParamDirection.Out)
        throw new Exception("HashTable " ~ direction.to!string ~ " parameters not supported");
    }
    else if (containerType != ContainerType.None)
    {
      if ((direction == ParamDirection.In && ownership != Ownership.None) || direction == ParamDirection.InOut)
        throw new Exception("Container " ~ containerType.to!string ~ " parameter with direction "
          ~ direction.to!string ~ " ownership " ~ ownership.to!string ~ " not supported");
    }

    if (containerType == ContainerType.None && kind == TypeKind.Basic && direction == ParamDirection.In
        && cType.countStars > 0 && dType != cType)
      throw new Exception(
          "Basic input parameter type '" ~ dType.to!string ~ "' has unexpected C type '"
          ~ cType.to!string ~ "'");

    if (lengthParamIndex != ArrayNoLengthParam) // Array has a length argument?
    {
      auto typeFunc = getParentByType!Func;

      if (lengthParamIndex >= typeFunc.params.length)
        throw new Exception("Invalid array length parameter index");

      auto lengthParam = typeFunc.params[lengthParamIndex];

      if ((direction == ParamDirection.In && lengthParam.direction == ParamDirection.Out)
          || (direction == ParamDirection.InOut && lengthParam.direction == ParamDirection.Out))
        throw new Exception("Array length parameter direction '" ~ to!string(
            lengthParam.direction) ~ "' is incompatible with array direction '" ~ direction.to!string ~ "'");
    }
  }

  private dstring _name; /// Name of parameter
  bool isInstanceParam; /// true if this parameter is the instance parameter
  bool isArrayLength; /// true if this parameter is an array length
  int arrayParamIndex; /// If isArrayLength is true, the parameter index of the array or ParamIndexReturnVal if the array is the return value
  ParamDirection direction; /// Parameter direction
  bool nullable; /// Nullable pointer
  bool optional; /// Optional pointer
  bool allowNone; /// Allow none (FIXME - how does this differ from nullable?)
  bool callerAllocates; /// Caller allocates value
  bool varargs; /// Indicates a parameter is a varargs ... elipsis

  int closureIndex = NoClosure; /// Closure parameter index
  int destroyIndex = NoDestroy; /// Destroy parameter index
  ParamScope scope_; /// FIXME
}

/// Value used for arrayParamIndex to indicate it is the length for the return value
enum ParamIndexReturnVal = -1;

/// Direction of a parameter
enum ParamDirection
{
  In = -1, /// Input direction (not actually found in Gir files, since it is the default unspecified value)
  Out, /// Output direction
  InOut, /// Input and output direction
}

immutable dstring[] ParamDirectionValues = ["out", "inout"];

/// Parameter scope
enum ParamScope
{
  Unset = -1,
  Call, /// FIXME
  Async, /// FIXME
  Notified, /// FIXME
}

immutable dstring[] ParamScopeValues = ["call", "async", "notified"];

enum NoClosure = -1;
enum NoDestroy = -1;
