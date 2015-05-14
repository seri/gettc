from topcoder.errors import UnsupportedType

def write(value, type):
    if type in ["int", "long", "float", "double", "boolean"]:
        return str(value)
    elif type == "char":
        return "'" + value + "'"
    elif type == "String":
        return '"' + value + '"'
    elif type.endswith("[]"):
        elem_type = type[:-2]
        return "[" + ", ".join(map(lambda elem: write(elem, elem_type), value)) + "]"
    raise UnsupportedType(type)
