class UnsupportedTypeError(Exception):
    def __init__(self, type):
        message = type + " is not a valid TopCoder type"
        Exception.__init__(self, message)
