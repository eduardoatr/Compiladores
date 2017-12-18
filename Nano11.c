#include <stdio.h>
int main(){
	int n, m, x;
	n = 1;
	m = 2;
	x = 5;
	while(x > n){
		n = n + m;
		printf("%d", n);
	}
}

