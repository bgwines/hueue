export PATH="$PATH:/home/ubuntu/.local/bin"

alias h="cd /home/ubuntu/hueue; .stack-work/dist/x86_64-linux/C\
abal-1.22.4.0/build/hueueui/hueueui"
alias w="cd /home/ubuntu/hueue; .stack-work/dist/x86_64-linux/C\
abal-1.22.4.0/build/webhook/webhook"

alias du="d; u"
alias dw="d; w"

function d() {
    cd /home/ubuntu/hueue;
    git pull --rebase=preserve origin master;
    stack build;
}

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias ebash="emacs ~/.bash_profile; source ~/.bash_profile"

alias c="clear"

