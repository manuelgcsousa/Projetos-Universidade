namespace CleanicaFinal
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    using System.ComponentModel.DataAnnotations.Schema;
    using System.Data.Entity.Spatial;

    public partial class Formulario_de_Satisfacao
    {
        [Key]
        public int id_FormularioS { get; set; }

        [Required]
        [StringLength(50)]
        public string Cliente_Utilizador_email { get; set; }

        [Required]
        [StringLength(50)]
        public string Funcionario_Utilizador_email { get; set; }

        [StringLength(100)]
        public string sugestoes { get; set; }

        public int? pontuacao { get; set; }

        [Required]
        [StringLength(1)]
        public string pendente { get; set; }

        public virtual Cliente Cliente { get; set; }

        public virtual Funcionario Funcionario { get; set; }
    }
}
