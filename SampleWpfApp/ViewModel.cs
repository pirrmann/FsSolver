using FsSolver;
using System;
using System.ComponentModel;

namespace SampleWpfApp
{
    class ViewModel : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        private SolverValue x;
        private SolverValue y;
        private SolverValue z;

        public SolverValue X { get { return x; } set { if (x != value) { x = value; OnChange("X"); } } }
        public SolverValue Y { get { return y; } set { if (y != value) { y = value; OnChange("Y"); } } }
        public SolverValue Z { get { return z; } set { if (z != value) { z = value; OnChange("Z"); } } }
        
        private void OnChange(string name)
        {
            var handler = PropertyChanged;
            if(handler != null)
            {
                handler(this, new PropertyChangedEventArgs(name));
            }

            var rules = SampleWpfAppRules.GetRules();
            var problem = BoundProblem.Create(rules, this);
            var incoherencies = problem.Solve();
        }
    }
}
