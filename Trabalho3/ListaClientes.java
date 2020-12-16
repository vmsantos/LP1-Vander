
public class ListaClientes {


		
		private RepositorioClientesArray clientes;
		private int indiceLista;
			
		public ListaClientes() {
			clientes = new RepositorioClientesArray();
			indiceLista = 0;
		}
		
		public boolean hasNext() {
			
			return  indiceLista < clientes.getIndice();
		}
		
		public Cliente next () {
			Cliente c = clientes.getClientes()[indiceLista];
			indiceLista = indiceLista + 1;
			return c;
		}
		
		public void insert (Cliente c) {
			clientes.inserir(c);
			
		}



}
