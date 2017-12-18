#include <stdio.h>
int main(){
	float preco, venda, novo_preco;
    printf("Digite o preco: ");
    scanf(preco);
    printf("Digite venda: ");
    scanf(venda);
    if((venda < 500.0) || (preco < 30.0)){
        novo_preco = preco + 10.0/100.0 * preco;
    }else{
        if((venda >= 500.0 && venda < 1200.0) || (preco >= 30.0 && preco < 80.0)){
            novo_preco = preco + 15.0/100.0 * preco;
        }else{
            if(venda >= 1200.0 || preco >= 80.0){
                novo_preco = preco - 20.0/100.0 * preco;
	     }	
	}
    }
    printf("O novo preco e", novo_preco,"\n");
}
