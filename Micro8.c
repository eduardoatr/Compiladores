#include <stdio.h>
int main(){
	int numero;
	numero = 1;
	while(numero != 0){
        printf("Digite um numero: ");
        scanf(numero);
        if(numero > 10){
            printf("O numero",numero,"e maior que 10\n");
        }else{
            printf("O numero",numero,"e menor que 10\n");
	}
    }
}
