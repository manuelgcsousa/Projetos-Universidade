package business;

import data.TrocaDAO;
import data.TurnoDAO;
import java.util.ArrayList;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class Docente extends Utilizador {
    private String nome;
    private String idUC;
    
    public Docente() {
        super();
        this.nome = "";
    }
    
    public Docente (String username, String password, String nome, String idUC) {
        super(username, password);
        this.nome = nome;
        this.idUC = idUC;
    }
    
    public String getNome() {
        return this.nome;
    }
    
    public String getidUC() {
        return this.idUC;
    }
    
    public void setNome (String nome) {
        this.nome = nome;
    }
    
    public void setidUC (String idUC) {
        this.idUC = idUC;
    }
    
    public int login(String user, String password) throws DadosInvalidosException {
        int flag = -1;
        if(user.equals(this.getUsername()) && password.equals(this.getPassword())){
            flag = 0;
        } else {
            throw new DadosInvalidosException("Dados Inválidos");
        }
        return flag;
    }

    public void efetuaTrocaD (String idUC,int idAluno, String tipoTurno) throws CapMaxeNenhumComFaltasException {
        //Falta Verificar se existe capacidade para o meter ou não e se sim se existem alunos com faltas acima do permitido
        int[] info = TurnoDAO.getidTurno_capTurno(idUC, tipoTurno);
        int capAtual = info[2];
        int capMax = info[1];
        ArrayList<Integer> alunosFaltosos = new ArrayList<Integer>();
        
        if (capAtual < capMax) {
            TrocaDAO.efetuaTrocaDocente(idUC, idAluno,tipoTurno);
        } else if (!(alunosFaltosos = TurnoDAO.getAlunosFaltosos(idUC,tipoTurno)).isEmpty()) {
            int idAlunoFaltoso = alunosFaltosos.get(0);
            TrocaDAO.efetuaTrocaDocenteFaltosos(idUC, idAluno, idAlunoFaltoso, tipoTurno);
        } else {
            throw new CapMaxeNenhumComFaltasException("Não foi possível efetuar a Troca");
        }
        
    }

}
