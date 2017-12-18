#include <stdio.h>
int main(){
	int x, num, intervalo;
	intervalo = 0;
	for(x = 1; x <= 5; x=x+1){
        	printf("Digite um numero: ");
        	scanf(num);
        	if (num >= 10){
            		if (num <= 150){
                		intervalo = intervalo + 1;
			}
		}
	}
	printf("Ao total, foram digitados", intervalo, "numeros no intervalo entre 10 e 150\n");
}
