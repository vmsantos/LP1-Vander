
public class ListaContas {
	
	private RepositorioContasArray contas;
	private int indice;
		
	public ListaContas() {
		contas = new RepositorioContasArray();
		indice = 0;
	}
	
	public boolean hasNext() {
		
		return  indice < contas.getIndice();
	}
	
	public Conta next () {
		Conta c = contas.getContas()[indice];
		indice = indice + 1;
		return c;
	}
	
	public void insert (Conta c) {
		contas.inserir(c);
		
	}

}
