#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import strformat

iterator `>..`(left, right: int): int =
  for num in countdown(left - 1, right):
    yield num

proc sochetanie(n,k: int): void =
  var a: seq[int] = @[-1]
  var incdir: seq[int] = @[-1]
  for i in 1 .. k:
    a.add i
    incdir.add 1

  while true:
    echo &"==={\" \":^50}===\n==={a[1..^1]:^50}===\n==={\" \":^50}==="

    var isMin: bool
    var searching = true # Ищем цифру?
    var i = k
    while searching:
      if i == 0:
        echo "Дошли до конца числа, все сочетания пройдены "
        return

      echo &"i = {i}, a[i] = {a[i]}, a: {a[1..^1]}"
      if i != 1:
        if a[i] - a[i - 1] == 1:
          isMin = true
          echo "- Число меньше предыдущего, следовательно минимальное"
        else:
          isMin = false
          echo "- Число не минимальное"
      else:
        isMin = true
        echo "- Первое число, гарантированно минимальное"

      # Так как числа расположены в порядке возрастания то максимально
      # возмножное число для позиции зависит от ее расположения и
      # общемго количества цифр.
      let isMax = a[i] == n - k + i

      searching = (incdir[i] == -1 and isMin) or
                  (incdir[i] == 1 and isMax)

      if searching:
        incdir[i] = if isMin: 1
                    else:     -1

      i -= 1

      echo "- Рассматриваем следующее число"

    echo &"- Нашли позицию для изменения: i = {i}"

    let A = a[i]
    a[i] = a[i] + incdir[i]
    for j in i+1 .. k:
      if a[j] - A == j - i:
          a[j] = a[i] + j - i

    echo "- Совершили изменение, повторяем цикл"


echo "---"
sochetanie(3,2)
echo "---"
