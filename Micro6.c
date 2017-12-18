#include <stdio.h>
int main(){
	int numero;
    printf("Digite um numero de 1 a 5: ");
    scanf(numero);
    switch(numero){
        case 1:
            printf("Um\n");
            break;
        case 2:
            printf("Dois\n");
            break;
        case 3:
            printf("Tres\n");
            break;
        case 4:
            printf("Quatro\n");
            break;
        case 5:
            printf("Cinco\n");
            break;
        default:
             printf("Numero invalido!\n");
    }
}
