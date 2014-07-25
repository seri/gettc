def with_env key, value
    saved = ENV[key]
    ENV[key] = value
    yield
    ENV[key] = saved
end

def with_dir dir
    saved = pwd
    chdir dir, { verbose: false }
    yield
    chdir saved, { verbose: false }
end