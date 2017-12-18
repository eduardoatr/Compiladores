#include <stdio.h>
int main(){
	int numero;
	printf("Digite um numero: ");
	scanf(numero);
	if(numero >= 100){
        	if(numero <= 200){
            		printf("O numero esta no intervalo entre 100 e 200\n");
        	}else{
            		printf("O numero nao esta no intervalo entre 100 e 200\n");
		}
    	}else{
        	printf("O numero nao esta no intervalo entre 100 e 200\n");
	}
}
