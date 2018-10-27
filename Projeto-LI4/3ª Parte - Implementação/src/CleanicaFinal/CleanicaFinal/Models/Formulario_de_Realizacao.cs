namespace CleanicaFinal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    public partial class Formulario_de_Realizacao
    {
        [Key]
        public int id_FormularioR { get; set; }

        [Required]
        [StringLength(50)]
        public string Cliente_Utilizador_email { get; set; }

        [Required]
        [StringLength(50)]
        public string FuncionarioUtilizador_email { get; set; }

        public int duracao { get; set; }

        [StringLength(100)]
        public string observacoes { get; set; }

        public virtual Cliente Cliente { get; set; }

        public virtual Funcionario Funcionario { get; set; }
    }
}
