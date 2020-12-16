public class Poupanca extends Conta{
	
	public Poupanca (String num, Cliente c) {
		super(num, c);		
	}

	public void renderJuros(double taxa) {
		double saldoAtual = getSaldo();
		creditar(saldoAtual * taxa);
	}

}