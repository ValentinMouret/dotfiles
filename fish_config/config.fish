set -gx LC_ALL "en_US.UTF-8"

status --is-interactive; and source (pyenv init -|psub)
for f in ~/.config/fish/conf.d/*.sh;
  source $f
end

set -gx PATH "$HOME/.jenv/bin:$PATH"
set -gx PATH "$PATH:/Users/valentin/go/src/github.com/bazelbuild/buildtools/bazel-out/darwin-fastbuild/bin/buildifier/darwin_amd64_stripped"

set -gx JAVA_HOME "/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home"

eval (starship init fish)

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/valentin/google-cloud-sdk/path.fish.inc' ]; . '/Users/valentin/google-cloud-sdk/path.fish.inc'; end

set -g RUBY_CONFIGURE_OPTS "--with-openssl-dir="(brew --prefix openssl@1.1)
status --is-interactive; and source (rbenv init -|psub)
