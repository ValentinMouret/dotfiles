set -gx LC_ALL "en_US.UTF-8"

pyenv init - | source

set -gx PATH "$HOME/.jenv/bin:$PATH"

set -gx JAVA_HOME "/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home"

set -g RUBY_CONFIGURE_OPTS "--with-openssl-dir="(brew --prefix openssl@1.1)
status --is-interactive; and source (rbenv init -|psub)

# Golang
set -gx GOPATH ~/go
set -gx GOBIN $GOPATH/bin
set -gx PATH $GOBIN $PATH
