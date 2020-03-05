#!/usr/bin/env bash

echo "Запущен файл $0 с аргументами [$*]"
echo "Запущен от пользователя $(echo $HOME | cut -d'/' -f3)"
echo "Путь содержит $(echo $PATH | tr ':' '\n' | wc -l) директорий"
echo "Тип терминала $TERM"
echo "Пользовательский шелл $SHELL"

if which pacman; then
    echo "В системе установлено (в явном виде) $(pacman -Qe | wc -l) пакетов"
    echo "В системе установлено $(pacman -Q | wc -l) пакетов"
fi

found=false

echo $PATH | tr : '\n' | xargs ls 2>/dev/null |
    while read -r name; do
        case "$name" in
            subl3)
                echo "found sublime-text editor"
                found=true
                ;;
            fish)
                echo "found fish shell"
                found=true
                ;;
            zsh)
                echo "found zsh shell"
                found=true
                ;;
            emacs)
                echo "found emacs text editor"
                found=true
                ;;
        esac
    done

if [ $found="true" ]; then
    echo -e "\e[32mОдна из искомых программ была найдена\e[0m"
else
    echo -e "\e[32mНи одна из искомых программ не была найдена\e[0m"
fi
