package business;

import data.TrocaDAO;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class Troca {
    private static int idTroca = TrocaDAO.ktsTrocas();
    private String turnoAtual;
    private String idUC;
    private String turnoPretendido;
    private int alunoReq; //idAluno
    private int alunoAceita; //idAluno
    
    public Troca(String turnoAtual, String turnoPretendido, int alunoReq, int alunoAceita, String idUC) {
        this.idTroca++;
        this.turnoAtual = turnoAtual;
        this.turnoPretendido = turnoPretendido;
        this.alunoReq = alunoReq;
        this.alunoAceita = alunoAceita;
        this.idUC = idUC;
    }

    public int getidTroca() {
        return this.idTroca;
    }

    public String getTurnoAtual() {
        return this.turnoAtual;
    }

    public String getTurnoPretendido() {
        return this.turnoPretendido;
    }

    public int getAlunoReq() {
        return this.alunoReq;
    }

    public String getidUC() {
        return this.idUC;
    }

    public int getAlunoAceita() {
        return this.alunoAceita;
    }
}
