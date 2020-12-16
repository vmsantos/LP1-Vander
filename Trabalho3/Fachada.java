public class Fachada {

	private static Fachada instancia;
	private CadastroContas contas;
	private CadastroClientes clientes;

	public static Fachada obterInstancia() {
		if (instancia == null) {
			instancia = new Fachada();
		}
		return instancia;
	}

	private Fachada() {
		initCadastros();
	}

	private void initCadastros() {
		RepositorioContasArray rep = new RepositorioContasArray();
		contas = new CadastroContas(rep);
		RepositorioClientesArray repClientes = new RepositorioClientesArray();
		clientes = new CadastroClientes(repClientes);
	}

	// metodos para manipular clientes
	public void atualizar(Cliente c) {
		clientes.atualizar(c);
	}

	public Cliente procurarCliente(String cpf) {
		return clientes.procurar(cpf);
	}

	public void cadastrar(Cliente c) {
		clientes.cadastrar(c);
	}

	public void descadastrarCliente(String cpf) {
		// descadastra primeiro as contas do cliente, pois nao faz sentido conta sem cliente
		// esta eh uma regra de negocio
		Conta conta = null;
		ListaContas lista_contas =  listaContasCliente(cpf);
		while (lista_contas.hasNext()) {
			conta = lista_contas.next();
			contas.descadastrar(conta.getNumero());
		}
		// agora sim, descadastra o cliente
		clientes.descadastrar(cpf);
	}

	// metodos para manipular contas
	public int atualizar(Conta c) {
		int r = -1;
		
		// pega a conta cadastrada com o numero informado pelo usuario
		Conta ct = contas.procurar(c.getNumero());
		if (ct != null) {
		// pega o cliente cadastrado com o cpf informado pelo usuario	
			Cliente cli = clientes.procurar(c.getCliente().getCpf());
			if (cli != null) {
				// muda o cliente da conta com o cliente que tem o cpf informado pelo usuario
				ct.setCliente(cli);
			}
		}
		// informa a mudanca ao cadastro: isto eh importante para fins de consistencia do estado da conta
		// numa aplicacao que eventualmente tivesse persistencia em banco de dados ou se 
		//fosse  uma aplicacao distribuida.
		r = contas.atualizar(ct);
		
		//retorna sucesso ou codigo de erro
		return r;
	}

	public Conta procurarConta(String n) {
		return contas.procurar(n);
	}

	public int cadastrar(Conta c) {
		int r = -1;
		Cliente cliente_lido, cliente_existente = null;
		cliente_lido = c.getCliente();
		if (cliente_lido != null) {
			cliente_existente = clientes.procurar(cliente_lido.getCpf());
			if (cliente_existente != null) {
				c.setCliente(cliente_existente);
				r = contas.cadastrar(c);
			} 
		} 
		return r;
	}

	public int descadastrarConta(String n) {
		return contas.descadastrar(n);
	}

	public int creditar(String n, double v) {
		return contas.creditar(n, v);
	}

	public int renderBonus(String n) {
		return contas.renderBonus(n);
	}

	public int renderJuros(String n, double v) {
		return contas.renderJuros(n, v);
	}

	public int debitar(String n, double v) {
		return contas.debitar(n, v);
	}

	public int transferir(String origem, String destino, double val) {
		return contas.transferir(origem, destino, val);
	}

	public ListaContas listaContasCliente(String cpf) {
		ListaContas lc = null;
		Cliente cliente = clientes.procurar(cpf);
		if (cliente != null) {
			lc = contas.listaContasCliente(cpf);
		} 
		return lc;
	}

	public ListaContas listaContas() {

		return contas.listaContas();
		
	}

	public ListaClientes listaClientes() {
		// 
		return clientes.listaClientes();
	}

}
