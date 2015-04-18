using FsSolver;
using System;
using System.ComponentModel;

namespace SampleWpfApp
{
    class ViewModel : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        private object lockObject = new object();
        private bool computing = false;

        private SolverValue x;
        private SolverValue y;
        private SolverValue z;

        public SolverValue X { get { return x; } set { if (x != value) { x = value; OnChange("X", value); } } }
        public SolverValue Y { get { return y; } set { if (y != value) { y = value; OnChange("Y", value); } } }
        public SolverValue Z { get { return z; } set { if (z != value) { z = value; OnChange("Z", value); } } }
        
        private void OnChange(string name, SolverValue value)
        {
            var handler = PropertyChanged;
            if(handler != null)
            {
                handler(this, new PropertyChangedEventArgs(name));
            }

            lock (lockObject)
            {
                if (computing)
                {
                    return;
                }

                computing = true;

                try
                {
                    bool useComputedValues = (value == null) || !(value.IsProvided || value.IsProvidedNoConflict);
                    var rules = SampleWpfAppRules.GetRules();
                    var problem = BoundProblem.Create(rules, this, useComputedValues);
                    var incoherencies = problem.Solve();
                    if (incoherencies.Length == 0 && !useComputedValues)
                    {
                        problem = BoundProblem.Create(rules, this, true);
                        problem.Solve();
                    }
                }
                finally
                {
                    computing = false;
                }
            }
        }
    }
}
