#include <stdio.h>
#define SIZE 4

void zero(char *a, int size)
{
	while (size>0)
		a[size--] = 0; 
}

void initialize(char *a, int size)
{
	zero(a, size);
}

void multiply(char *a, int size, int mult)
{
	int i;
	for (i=0; i<size; i++)
		a[i] = i * mult;
}

void pr_array(char *a, int size)
{
	int i;
	for (i=0; i<size; i++)
		printf("f(%d)=%d\n", i, a[i]);
}

int main(int argc, char **argv)
{
	char a[SIZE];
	int mult = 2;

	initialize(a, SIZE);
	multiply(a, SIZE, mult);
	pr_array(a, SIZE);
	return 0;
}
