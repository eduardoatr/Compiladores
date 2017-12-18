#include <stdio.h>
int main(){
    char nome, sexo;
	int x, h, m;
	h = 0;
	m = 0;
	for(x = 1; x <= 5; x=x+1){
		printf("Digite o nome: ");
		scanf(nome);
		printf("H - Homem ou M - Mulher: ");
		scanf(sexo);
		switch(sexo){
		    case 'H':
		        h = h + 1;
		        break;
		    case 'M':
		        m = m + 1;
		        break;
		    default:
		         printf("Sexo so pode ser H ou M!\n");
		}
	}
	printf("Foram inseridos",h,"Homens\n");
	printf("Foram inseridas",m,"Mulheres\n");
}
