public class Cliente {

    private String nome;
    
    private String cpf;
    
    

	public Cliente(String cpf, String nome) {

	        this.nome = nome;
        	this.cpf = cpf;
        
	}


	public String getCpf() {
		return cpf;
	}


	public String getNome() {
		return nome;
	}


	public void setCpf(String string) {
		cpf = string;
	}


	public void setNome(String string) {
		nome = string;
	}

}
