public class CadastroClientes {

	private RepositorioClientesArray clientes;

	public CadastroClientes(RepositorioClientesArray r) {
		this.clientes = r;
	}

	public void atualizar(Cliente c) {
		 Cliente cli = clientes.procurar(c.getCpf());
		 if (cli != null) {
			 cli.setNome(c.getNome());
			 clientes.atualizar(cli);
		 }
	}

	public void cadastrar(Cliente c) {
		if (!clientes.existe(c.getCpf())) {
			clientes.inserir(c);
		} else {
			InterfaceTextual.mostra_mensagem("Cliente já cadastrado");
		}
	}

	public void descadastrar(String n) {
		clientes.remover(n);
	}

	public Cliente procurar(String n) {
		return clientes.procurar(n);
	}

	public ListaClientes listaClientes() {

		Cliente[] clientes = null;
		Cliente cliente = null;
		clientes = this.clientes.getClientes();
		int numeroClientes = this.clientes.getIndice();
		ListaClientes lc = new ListaClientes();
		for (int i = 0; i < numeroClientes; i++) {
			cliente = clientes[i];
			lc.insert(cliente);
		}
		return lc;

	}

}
