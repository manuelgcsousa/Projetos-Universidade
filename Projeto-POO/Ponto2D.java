import static java.lang.Math.abs;
import java.io.*;

public class Ponto2D implements Serializable {

    //VARIÁVEIS DE INSTANCIA
    private double x, y;

    //CONSTRUTORES USUAIS
    public Ponto2D (double cx, double cy) { x = cx; y = cy; }

    public Ponto2D () { this(0.0, 0.0); }

    public Ponto2D (Ponto2D p) { x = p.getX(); y = p.getY(); }

    //MÉTODOS DA INSTÂNCIA
    public double getX() { return x; } 

    public double getY() { return y; }

    //INCREMENTOS DAS COORDENADAS
    public void incCoord (double dx, double dy) {
        x += dx;
        y += dy;
    } 

    //DECREMENTO DAS COORDENADAS
    public void decCoord (double dx, double dy) {
        x -= dx;
        y -= dy;
    } 

    //SOMA OS VALORES DE UM PARAMETRO E DEVOLVE UM NOVO PONTO
    public Ponto2D somaPonto (double dx, double dy) {
        return new Ponto2D (x + dx, y + dy);
    }

    //VERIFICA SE OS DOIS PONTOS SÃO SIMÉTRICOS (SE OS EIXOS XX DISTAM O MESMO DOS EIXOS YY)
    public boolean simetrico () {
        return abs(x) == abs(y);
    }

    //VERIFICA SE AS COORDENADAS SÃO POSITIVAS
    public boolean coordPos () {
        return x > 0 && y > 0;
    }
    
    //VERIFICA SE DOIS PONTOS SÃO IGUAIS
    public boolean equals (Object o) {
       if (this == o) return true;
       
       if ((o == null) || (this.getClass() != o.getClass())) return false;
       
       Ponto2D aux = (Ponto2D) o;
       return (this.x == aux.getX() && this.y == aux.getY());
   }

    //CONVERTE PARA REPRESENTAÇÃO TEXTUAL
    public String toString () {
        return new String ("Pt2D = " + x + ", " + y);
    }

    //CRIA UMA COPIA DO PONTO RECEPTOR (RECEPTOR = this)
    public Ponto2D clone () {
        return new Ponto2D (this);
    }
}
