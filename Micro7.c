#include <stdio.h>
int main(){
	int programa, numero;
	char opc;
	programa = 1;
	while(programa == 1){
        printf("Digite um numero: ");
        scanf(numero);
        if(numero > 0){
            printf("Positivo\n");
        }else{
            if(numero == 0){
                printf("O numero e igual a zero\n");
		}
            if(numero < 0){
                printf("Negativo\n");
		}
	}
        printf("Deseja finalizar? (S/N)\n");
        scanf(opc);

        if(opc == "S"){
            programa = 0;
	}
    }
}
