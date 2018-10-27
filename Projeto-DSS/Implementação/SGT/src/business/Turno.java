package business;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */
public class Turno {
    private int idTurno;
    private String tipo;
    private String sala;
    private int capacidadeAtual;
    private int capacidadeMax;
    private String idUC;
    private Map<Integer, Integer> faltas;
    
    public Turno (int idTurno, String tipo, String sala, int capacidadeAtual, int capacidadeMax, String idUC) {
        this.idTurno = idTurno;
        this.tipo = tipo;
        this.sala = sala;
        this.capacidadeAtual = capacidadeAtual;
        this.capacidadeMax = capacidadeMax;
        this.idUC = idUC;
        this.faltas = new HashMap<Integer, Integer>();
    }
    
    public Turno(int idTurno, String tipo, String sala, int capacidadeAtual, 
                 int capacidadeMax, String idUC, Map<Integer, Integer> faltas) {
        this.idTurno = idTurno;
        this.tipo = tipo;
        this.sala = sala;
        this.capacidadeAtual = capacidadeAtual;
        this.capacidadeMax = capacidadeMax;
        this.idUC = idUC;
        this.faltas = faltas;
    }
    
    public Turno (Turno t) {
        this.idTurno = t.getidTurno();
        this.tipo = t.getTipo();
        this.sala = t.getSala();
        this.capacidadeAtual = t.getCapacidadeAtual();
        this.capacidadeMax = t.getCapacidadeMax();
        this.idUC = t.getidUC();
        this.faltas = t.getFaltas();
    }
    
    /**
     * Getters e Setters.
     * @return 
     */
    public int getidTurno() {
        return this.idTurno;
    }
    
    public String getTipo() {
        return this.tipo;
    }
    
    public String getSala() {
        return this.sala;
    }
    
    public int getCapacidadeAtual() {
        return this.capacidadeAtual;
    }
    
    public int getCapacidadeMax() {
        return this.capacidadeMax;
    }
    
    public String getidUC() {
        return this.idUC;
    }
    
    public Map<Integer, Integer> getFaltas() {
        return this.faltas;
    }
    
    /**
     * Método auxilar clone().
     * Retorna uma cópia do respetivo objeto.
     * @return 
     */
    public Turno clone() {
        return new Turno(this);
    }
}

