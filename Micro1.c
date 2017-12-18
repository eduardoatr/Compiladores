#include <stdio.h>
int main(){
	float cel, far;
	printf("Tabela de conversao: Celsius -> Fahrenheit\n");
	printf("Digite a temperatura em Celsius: ");
	scanf(cel);
	far = (9.0*cel+160.0)/5.0;
	printf("A nova temperatura eh:",far);
}

