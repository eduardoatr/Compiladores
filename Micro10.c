#include <stdio.h>
int fatorial(int n){
    if(n <= 0){
        return 1;
	}
    else{
        return n * fatorial(n - 1);
	}
}
int main(){
    int numero, fat;
    printf("Digite um numero: ");
    scanf(numero);
    fat = fatorial(numero);
    printf("O fatorial de", numero,"eh", fat);
}
