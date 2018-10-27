package business;

import data.AlunoDAO;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class Aluno extends Utilizador {
    private String nome;
    private int numero;
    private String estatuto;
    private Map<String, String> turnos; //String => idUC

    public Aluno() {
        super();
        this.nome = "";
        this.numero = 0;
        this.estatuto = "";
        this.turnos = new HashMap<String, String>();
    }

    public Aluno (String username, String password,
                  String nome, int numero, String estatuto)
    {
        super(username, password);
        this.nome = nome;
        this.numero = numero;
        this.estatuto = estatuto;
        this.turnos = new HashMap<String, String>();
    }
    
    public int getNumero() {
        return this.numero;
    }

    public String getNome() {
        return this.nome;
    }

    public String getEstatuto() {
        return this.estatuto;
    }

    public Map<String, String> getTurnos() {
        return this.turnos;
    }

    public void setTurnos (Map<String, String> turnos) {
        this.turnos = turnos;
    }

    public int login (String user, String password) throws DadosInvalidosException {
        int flag = -1;
        if (user.equals(this.getUsername()) && password.equals(this.getPassword())) {
            flag = 1;
        } else {
            throw new DadosInvalidosException("Dados Inválidos");
        }
        return flag;
    }

    public void registaAluno() {
        AlunoDAO.put(this.numero, this);
    }
}
