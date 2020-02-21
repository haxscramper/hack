#!/usr/bin/env bash

echo "Запущен файл $0 с аргументами [$*]"
echo "Запущен от пользователя $(echo $HOME | cut -d'/' -f3)"
echo "Путь содержит $(echo $PATH | tr ':' '\n' | wc -l) директорий"
echo "Тип терминала $TERM"
echo "Пользовательский шелл $SHELL"
