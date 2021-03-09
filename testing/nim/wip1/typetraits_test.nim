import std/[typetraits]

echo typeof array[0 .. 3, int]
echo genericParams array[0 .. 3, int]
echo genericParams(array[0 .. 3, int]).get(0)
echo genericParams(array[0 .. 3, int]).get(0) is range
