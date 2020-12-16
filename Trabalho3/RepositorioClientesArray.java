

public class RepositorioClientesArray {
	private Cliente[]  clientes;
	private int indice;
	private final static int tamanhoCache = 100;

	public RepositorioClientesArray() {
	  indice = 0;
	  clientes = new Cliente[tamanhoCache];
	}
  
     public Cliente [] getClientes () {
       return clientes;
     }

     public int getIndice() {
       return indice;
     }

	public void atualizar(Cliente c){
	  int i = procurarIndice(c.getCpf());
	  if (i != -1) {
	    clientes[i] = c;
	  } else {
		  InterfaceTextual.mostra_mensagem("Cliente nao encontrado");
	  }
	}
	public boolean existe(String cpf) {
	  boolean resp = false;

	  int i = this.procurarIndice(cpf);
	  if(i != -1){
		resp = true;
	  }

	  return resp;
	}
	public void inserir(Cliente c){
	    clientes[indice] = c;
	    indice = indice + 1;
	}
	public Cliente procurar(String cpf){
		
	  Cliente c = null;
	  if (existe(cpf)) {
  	    int i = this.procurarIndice(cpf);
	    c = clientes[i];
	  } else {
		  InterfaceTextual.mostra_mensagem("Cliente nao encontrado");
	  }

	  return c;
	}

	private int procurarIndice(String cpf) {
	  int     i = 0;
	  int     ind = -1;
	  boolean achou = false;

	  while ((i < indice) &&!achou) {
	    if ((clientes[i].getCpf()).equals(cpf)) {
		ind = i;
		achou = true;
	    }
	    i = i + 1;
	  }
	  return ind;
	}

	public void remover(String cpf){
	  if (existe(cpf)) {
  	    int i = this.procurarIndice(cpf);
	    clientes[i] = clientes[indice - 1];
	    clientes[indice - 1] = null;
	    indice = indice - 1;
	  } else {
		  InterfaceTextual.mostra_mensagem("Cliente nao encontrado");
	  }
	}
}