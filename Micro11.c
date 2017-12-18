#include <stdio.h>
int verifica(int n){
    int res;
    if (n > 0){
        res = 1;
    }else{
        if (n < 0){
            res = -1;
        }else{
            res = 0;
	}
    }
    return res;
}
int main(){
    int numero, x;
    printf("Digite um numero: ");
    scanf(numero);
    x = verifica(numero);
    if (x == 1){
        printf("Numero positivo\n");
    }else{
        if (x == 0){
            printf("Zero\n");
        }else{
            printf("Numero negativo\n");
	}
    }
}
