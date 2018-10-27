package data;

import business.Troca;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Carlos Pedrosa
 * @author David Sousa
 * @author Manuel Sousa
 */

public class TrocaDAO {
    
    public static int ktsTrocas() {
        int kts = 0;
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT count(*) as count FROM Trocas");
            ResultSet rs = stm.executeQuery();
            
            if (rs.next()) { 
                kts = rs.getInt("count");
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return kts;
    }
    
    public static List<Troca> get (String idUC) {
        List<Troca> ltr = new ArrayList<Troca>();
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            PreparedStatement stm = conn.prepareStatement("SELECT * FROM Trocas WHERE idUC=? and alunoAceita = -1");
            stm.setString(1, idUC);
            ResultSet rs = stm.executeQuery();
            
            while (rs.next()) { 
                int idTroca = rs.getInt("idTrocas");
                String turnoAtual = rs.getString("turnoAtual");
                String turnoPretendido = rs.getString("turnoPretendido");
                int alunoReq = rs.getInt("alunoReq");
                int alunoAceita = -1;
                Troca tr = new Troca(turnoAtual, turnoPretendido, alunoReq, alunoAceita, idUC);
                ltr.add(tr);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return ltr;
    }

    public static void put (Troca troca) {
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            
            int count = trocaExiste(troca);
            System.out.println(count);
            
            if (count == 0) {
                PreparedStatement stm = conn.prepareStatement("INSERT INTO Trocas\n" +
                    "VALUES (?, ?, ?, ?, ?, ?)\n");
                stm.setInt(1, troca.getidTroca());
                stm.setString(2, troca.getTurnoAtual());
                stm.setString(3, troca.getTurnoPretendido());
                stm.setInt(4, troca.getAlunoReq());
                stm.setInt(5, troca.getAlunoAceita());
                stm.setString(6, troca.getidUC());
                stm.executeUpdate();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            Connect.close(conn);
        }
    }
    
    public static void aceitaTrocaCompleta (String idUC,
                                            int idAlunoReq,
                                            int idAlunoAceita, 
                                            String tipoTurnoAtual, 
                                            String tipoTurnoPretendido) 
    {   
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            int idTurnoAtual = -1;
            int idTurnoPretendido = -1;
           
            /**
             * Selecionar os idTurno's referentes aos tipoTurno's dados.
             * 
             * SQL_2 => idTurno referente ao tipoTurnoAtual.
             * SQL_3 => idTurno referente ao tipoTurnoPretendido.
             */
            
            //SQL_2:
            String SQL_2 = "SELECT idTurno FROM Turno " + 
                           "WHERE tipoTurno = ? and UC_idUC = ?";
            PreparedStatement stm2 = conn.prepareStatement(SQL_2);
            stm2.setString(1, tipoTurnoAtual);
            stm2.setString(2, idUC);
            
            ResultSet rs2 = stm2.executeQuery();
            if (rs2.next()) idTurnoAtual = rs2.getInt("idTurno");
            
            //SQL_3:
            String SQL_3 = "SELECT idTurno FROM Turno " + 
                           "WHERE tipoTurno = ? and UC_idUC = ?";
            PreparedStatement stm3 = conn.prepareStatement(SQL_3);
            stm3.setString(1, tipoTurnoPretendido);
            stm3.setString(2, idUC);
            
            ResultSet rs3 = stm3.executeQuery();
            if (rs3.next()) idTurnoPretendido = rs3.getInt("idTurno");
            
            
            /**
             * Fazer Update na tabela Aluno_has_Turno e Trocas.
             * Mudar os turnos de cada um dos alunos envolvidos na troca, 
             * e atualizar informação da Troca em questão.
             * 
             * SQL_4 => Update ao aluno que aceitou a troca.
             * SQL_5 => Update ao aluno que solicitou a troca.
             * SQL_6 => Update à tabela trocas, atualizando informação da troca.
             */
            
            //SQL_4:
            String SQL_4 = "UPDATE Aluno_has_Turno " + 
                           "SET Turno_idTurno = ? " + //idTurnoPretendido
                           "WHERE Aluno_idAluno = ? and Turno_idTurno = ?";
            PreparedStatement stm4 = conn.prepareStatement(SQL_4);
            stm4.setInt(1, idTurnoPretendido);
            stm4.setInt(2, idAlunoAceita);
            stm4.setInt(3, idTurnoAtual);
            stm4.executeUpdate();
            
            //SQL_5:
            String SQL_5 = "UPDATE Aluno_has_Turno " + 
                           "SET Turno_idTurno = ? " + //idTurnoAtual
                           "WHERE Aluno_idAluno = ? and Turno_idTurno = ?";
            PreparedStatement stm5 = conn.prepareStatement(SQL_5);
            stm5.setInt(1, idTurnoAtual);
            stm5.setInt(2, idAlunoReq);
            stm5.setInt(3, idTurnoPretendido);
            stm5.executeUpdate();
            
            //SQL_6:
            String SQL_6 = "UPDATE Trocas " + 
                           "SET alunoAceita = ? " + //idTurnoAtual
                           "WHERE turnoAtual = ? and turnoPretendido = ? and alunoReq = ? and idUC = ?";
            PreparedStatement stm6 = conn.prepareStatement(SQL_6);
            stm6.setInt(1, idAlunoAceita);
            stm6.setString(2, tipoTurnoPretendido);
            stm6.setString(3, tipoTurnoAtual);
            stm6.setInt(4, idAlunoReq);
            stm6.setString(5,idUC);
            stm6.executeUpdate();
            System.out.println(stm6);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
    
    public static void efetuaTrocaDocente (String idUC, int idAluno, 
                                           String turnoPretendido) 
    {
        Connection conn = null;
        
        try {
            conn = Connect.connect();

            PreparedStatement st = conn.prepareStatement("UPDATE Aluno_Has_Turno\n" + 
                    "SET Aluno_idAluno = ?, Turno_idTurno = ?, faltas = 0\n" + 
                        "Where Aluno_idAluno = ? and Turno_idTurno = ?");
            
            int idTurno = TurnoDAO.getidTurno_capTurno(idUC, turnoPretendido)[0];
            int idTurnoAntigo = TurnoDAO.getTurnoAntigo(idUC, idAluno);
                    
            st.setInt(1, idAluno);
            st.setInt(2, idTurno);
            st.setInt(3,idAluno);
            st.setInt(4, idTurnoAntigo);
            st.executeUpdate();    
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }

    private static int trocaExiste (Troca troca) {
        Connection conn = null;
        int count = -1;
        
        try {
            conn = Connect.connect();
                       
            PreparedStatement stm = conn.prepareStatement("Select count(*) as count from Trocas\n" +
                "Where TurnoAtual = ? and TurnoPretendido = ? and alunoReq = ? and idUC = ?\n");

            stm.setString(1, troca.getTurnoAtual());
            stm.setString(2, troca.getTurnoPretendido());
            stm.setInt(3, troca.getAlunoReq());
            stm.setString(4, troca.getidUC());
            ResultSet rs = stm.executeQuery();
            
            if (rs.next()) { 
                count = rs.getInt("count");
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
        return count;
    }

    public static void efetuaTrocaDocenteFaltosos (String idUC, int idAluno, 
                                                   int idAlunoFaltoso, 
                                                   String tipoTurno) 
    {
        Connection conn = null;
        
        try {
            conn = Connect.connect();
            
            int idTurno = TurnoDAO.getidTurno_capTurno(idUC, tipoTurno)[0];
            int idTurnoAntigo = TurnoDAO.getTurnoAntigo(idUC, idAluno);
            
            /* Novo Turno do Aluno que pediu a troca é o que ele pediu. */
            PreparedStatement stm = conn.prepareStatement("UPDATE Aluno_has_Turno\n" +
                    "SET Turno_idTurno = ? \n" +
                    "Where Aluno_idAluno = ? and Turno_idTurno = ?\n");
            stm.setInt(1, idTurno);
            stm.setInt(2, idAluno);
            stm.setInt(3, idTurnoAntigo);
            stm.executeUpdate();
            
            /* Novo Turno do Aluno Faltoso é o antigo do Aluno que pediu a Troca. */ 
            PreparedStatement st = conn.prepareStatement("UPDATE Aluno_has_Turno\n" +
                    "SET Turno_idTurno = ?\n" +
                    "Where Aluno_idAluno = ? and Turno_idTurno = ?\n");
            stm.setInt(1, idTurnoAntigo);
            stm.setInt(2, idAlunoFaltoso);
            stm.setInt(3, idTurno);
            stm.executeUpdate();          
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            Connect.close(conn);
        }
    }
}
