package business;

import com.mysql.jdbc.exceptions.MySQLIntegrityConstraintViolationException;
import data.UnidadeCurricularDAO;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class UnidadeCurricular {
    private String idUc;
    private String nome;
    private int nAulasPrevistas;
    private List<Turno> turnos;
    
    public UnidadeCurricular(String idUc, String nome, int nAulasPrevistas) {
        this.idUc = idUc;
        this.nome = nome;
        this.nAulasPrevistas = nAulasPrevistas;
        this.turnos = new ArrayList<Turno>();
    }
    
    public String getidUC() {
        return this.idUc;
    }
    
    public String getNome() {
        return this.nome;
    }
    
    public List<Turno> getTurnos() {
        return this.turnos;
    }
    
    public int getnAulasPrevistas(){
        return this.nAulasPrevistas;
    }
    
    public void setTurnos (List<Turno> turnos) {
        this.turnos.clear();
        for (Turno t : turnos) {
            this.turnos.add(t.clone());
        }
    }

    public boolean registaUC (int nTurnos) {
        boolean flag = true;
        
        try {
            UnidadeCurricularDAO.put(this, nTurnos);
        } catch (MySQLIntegrityConstraintViolationException e) {
            flag = false;
        }
        
        return flag;
    }
}
