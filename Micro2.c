#include <stdio.h>
int main(){
	int num1, num2;
	printf("Digite o primeiro numero: ");
	scanf(num1);
	printf("Digite o segundo numero: ");
	scanf(num2);
	if (num1 > num2){
        	printf("O primeiro numero", num1, "e maior que o segundo", num2);
   	}else{
        	printf("O segundo numero", num2, "e maior que o primeiro", num1);
	}
}


