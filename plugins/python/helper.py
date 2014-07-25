def deep_equals(x, y):
    if type(x) != type(y):
        return False
    if isinstance(x, list):
        if len(x) != len(y):
            return False
        for i, v in enumerate(x):
            if not deep_equals(v, y[i]):
                return False
        return True
    return x == y