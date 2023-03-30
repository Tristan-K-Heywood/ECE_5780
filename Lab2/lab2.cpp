#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath> // for ceil()
#include <algorithm> // for min()

using namespace std;
enum State { Idle, Released, Running, Preempted };
const string BORDER = "***************************************************************************************************";


class Task
{
   public:
      char name;
      int e, T, r, ttc;
      State state;
   
   
      Task( ) { }

      Task(char name, int e, int T, int r) // Periodic
      {
         this->name = name;
         this->e = e;
         this->T = T;
         this->r = r;
         this->ttc = e;
         state = Idle;
      }

      Task(char name, int e, int r) // Aperiodic
      {
         this->name = name;
         this->e = e;
         this->T = 0;
         this->r = r;
         this->ttc = e;
         state = Idle;
      }

      friend ostream & operator <<(ostream &out, Task& t)
      {
         out << "Task " << t.name << ": " << t.e << "/" << t.T << " @ " << t.r;
         return out;
      }

      void reset()
      {
         ttc = e;
      }
};


int idxOf(char* names, char name, int size)
{
   for(int i=0; i<size; ++i)
   {
      if(names[i] == name)
         return i;
   }
   return -1;
}


int idxFromID(Task* taskList, char taskID, int size)
{
   for(int i=0; i<size; ++i)
   {
      if(taskList[i].name == taskID)
         return i;
   }
   return -1;
}


void rmsSchedule(Task* pTasks, int numPTasks, Task* apTasks, int numApTasks, int simTime, char* outputFile)
{
   ofstream output;
   output.open(outputFile);
   stringstream line;

   output << "BEGIN RMS SIMULATION" << endl;
   // Assign RMS priorities
   char PI[numPTasks] = { }; // for simplified prio determination and LUT

   for(int i=numPTasks-1; i>=0; i--) // For each task in the priority queue
   {
      int lowestPrioI = 0; // Arbitrarily assign A as lowest priority

      for(int j=0; j<numPTasks; j++) // For each task
      {
         // If the new task is lower priority  AND it isn't already prioritized
         if(pTasks[j].T >= pTasks[lowestPrioI].T && idxOf(PI, pTasks[j].name, numPTasks) == -1)
         {
            lowestPrioI = j; // update the lowest priority
         }
      }
      PI[i] = pTasks[lowestPrioI].name; // by now, lowest priority gets its badge
   } // priorities are set


   char pastTaskID = '@';
   // For each 1ms time block
   for(int ms=0; ms < simTime; ms++) // one block iterations
   {
      line.str(std::string());
      bool sig = false;
      line << ms;


      // Update available tasks
      for(int t=0; t < numPTasks; t++) // for each periodic task
      {  
         Task cur = pTasks[t];
         if(cur.state == Idle && (ms%cur.T) == 0) // If task is comlete and a new period has arised
         {
            if(cur.ttc > 0 && ms != 0) // if it's incomplete, it missed a deadline
            {
               line << ": Task " << cur.name << " missed their deadline. Reset task.";
               sig = true;
            }
            pTasks[t].state = Released; // change current state
            pTasks[t].reset(); // reset the time to completion
         }
      }

      for(int a=0; a < numApTasks; a++) // for each aperiodic task
      {
         Task cur = apTasks[a];
         if(ms >= cur.r && cur.ttc > 0) // if we are past the release time and the task isn't finished
         {
            apTasks[a].state = Released; // the task must be released
         }
      }


      // need to signal preemption;
      int curTaskIdx;
      int highestPrio = numPTasks;
      char curTaskID = '+';
      bool isPeriodic = true;
      for(int i=0; i<numPTasks; ++i) // find list index of highest priority periodic task available
      {
         int cPrio = idxOf(PI, pTasks[i].name, numPTasks); // get priority of current periodic task
         // If current task is ready to run and a lower priority 
         if(pTasks[i].state != Idle && cPrio <= highestPrio)
         {
            curTaskIdx = i; // we have a new task to execute
            highestPrio = cPrio;
         }
      }

      if(highestPrio < numPTasks)
      {
         pTasks[curTaskIdx].state = Running;
         pTasks[curTaskIdx].ttc--;
         curTaskID = pTasks[curTaskIdx].name;
      }
      else
      {
         isPeriodic = false;
         // grab the first aperiodic task available
         for(int i=0; i<numApTasks; ++i)
         {
            // if the aperiodic task is available
            if(apTasks[i].state != Idle)
            {
               // choose it for running
               apTasks[i].state = Running;
               apTasks[i].ttc--;
               curTaskID = apTasks[i].name;
               break;
            }
         }
      }
      

      if(curTaskID != pastTaskID)
      {
         int i = idxFromID(pTasks, pastTaskID, numPTasks);
         if(i != -1 && pTasks[i].ttc != 0)
         {
            line << ": " << pastTaskID << " was preempted with " << pTasks[i].ttc << " cycles left";
            sig = true;
            pTasks[i].state = Preempted;
         }
         else 
         {
            i = idxFromID(apTasks, pastTaskID, numApTasks);
            if(i != -1 && apTasks[i].ttc != 0)
            {
               line << ": " << pastTaskID << " was preempted with " << apTasks[i].ttc << " cycles left";
               sig = true;
               apTasks[i].state = Preempted;
            }
            
         }
         if(pastTaskID == '@')
         {
            line << ": Started with task " << curTaskID;
            sig = true;
         }
         else if(curTaskID == '+')
         {
            line << ": Waiting for tasks to become available...";
            sig = true;
         }
         else
         {
            line << ": Started task " << curTaskID;
            sig = true;
         }
      }

      
      // Update idle tasks
      for(int t=0; t < numPTasks; t++) // for each periodic task
      {  
         Task cur = pTasks[t];
         if(cur.ttc <= 0 && cur.state != Idle) // If task is comlete and a new period has arised
         {
            pTasks[t].state = Idle; // change current state
            line << ": Task " << cur.name << " finished";
            sig = true;
         }
      }

      for(int a=0; a < numApTasks; a++) // for each aperiodic task
      {
         Task cur = apTasks[a];
         if(cur.ttc <= 0 && ms >= cur.r && cur.state != Idle) // if we are past the release time and the task is finished
         {
            apTasks[a].state = Idle; // the task must be made Idle
            line << ": Task " << cur.name << " finished";
            sig = true;
         }
      }


      // only output interesting iteration lines
      if(sig)
      {
         line << endl;
         output << line.str();
      }
      pastTaskID = curTaskID;
   }

   output << "End of RMS Simulation" << endl << endl << BORDER << endl << endl;
   output.close();
   return;
}


Task* copy(Task* list, int size, bool isPeriodic)
{
   Task* out = new Task[size];

   for(int i=0; i<size; ++i)
   {
      Task cur = list[i];
      if(isPeriodic)
      {
         out[i] = Task(cur.name, cur.e, cur.T, cur.r);
      }
      else
      {
         out[i] = Task(cur.name, cur.e, cur.r);
      }
   }

   return out;
}


void edfSchedule(Task* pTasks, int numPTasks, Task* apTasks, int numApTasks, int simTime, char* outputFile)
{
   ofstream output;
   output.open(outputFile, ios_base::app | ios_base::out);
   stringstream line;
   bool sig = false;

   output << "BEGIN EDF SIMULATION" << endl;

   char prevTaskName = '@';
   int prevTaskIndex = -1;
   bool prevIsPeriodicTask;


   for(int ms = 0; ms < simTime; ++ms)
   {
      line.str(std::string()); // reset the output line
      bool sig = false; // flag to only output interesting lines
      line << ms;
      // Update available tasks
      for(int t=0; t < numPTasks; ++t) // for each periodic task
      {  
         Task cur = pTasks[t];
         if(cur.state == Idle && (ms%cur.T) == 0) // If task is comlete and a new period has arised
         {
            if(cur.ttc > 0 && ms != 0) // if it's incomplete, it missed a deadline
            { 
               line << ": Task " << cur.name << " missed their deadline. Reset task.";
               sig = true;
            }
            pTasks[t].state = Released; // change current state
            pTasks[t].reset(); // reset the time to completion
         }
      }

      for(int a=0; a < numApTasks; a++) // for each aperiodic task
      {
         Task cur = apTasks[a];
         if(ms >= cur.r && cur.ttc > 0) // if we are past the release time and the task isn't finished
         {
            apTasks[a].state = Released; // the task must be released
         }
      }


      // Find earliest deadline from pool
      bool isPeriodicTask = false;
      int earliestIndex = -1;
      int earliestDeadline = 999;
      char earliestName = '+';
      
      // within the periodic tasks
      for(int i = 0; i < numPTasks; ++i)
      {
         if(pTasks[i].state != Idle) // only active tasks are selected for exec
         {
            int deadline;
            if(ms % pTasks[i].T == 0) // fix next periodic deadline discrepency when on a multiple of the period
            {
               deadline = (int) ceil((float) (ms+1) / (float) pTasks[i].T) * pTasks[i].T; // deadline is next multiple of period
            }
            else
            {
               deadline = (int) ceil((float) ms / (float) pTasks[i].T) * pTasks[i].T;
            }
            if(deadline < earliestDeadline) // if the current task's deadline is earlier
            {
               isPeriodicTask = true; earliestIndex = i; earliestName = pTasks[i].name;
               earliestDeadline = deadline; // update which task has the highest EDF priority
            }
         }
      }

      // within the *A*periodic tasks
      for(int i = 0; i < numApTasks; ++i)
      {
         if(apTasks[i].state != Idle) // only active tasks are selected for exec
         {
            int deadline = min(simTime, apTasks[i].r + 500); // deadline is end of simulation or 500 ms after release time, whichever comes first
            if(deadline < earliestDeadline) // if the current tasks's deadline is earlier
            {
               isPeriodicTask = false; earliestIndex = i; earliestName = apTasks[i].name;
               earliestDeadline = deadline; // update which tassk has the highest EDF priority
            }
         }
      }

      
      // execute current task
      if(isPeriodicTask) // is periodic
      {
         pTasks[earliestIndex].state = Running;
         pTasks[earliestIndex].ttc--;
         // executed task name already stored in earliestTaskID
      }
      else // is *A*periodic
      {
         apTasks[earliestIndex].state = Running;
         apTasks[earliestIndex].ttc--;
         // executed task name already stored in earliestTaskID
      }


      if(earliestName != prevTaskName) // switching tasks cases
      {
         if(prevTaskName != '@' && prevTaskName != '+') // placeholders do not get preempted
         {
            if(prevIsPeriodicTask && pTasks[prevTaskIndex].ttc > 0) // a periodic task was preempted
            {
               line << ": " << prevTaskName << " was preempted with " << pTasks[prevTaskIndex].ttc << " cycles left";
               pTasks[prevTaskIndex].state = Preempted; 
               sig = true;
            }
            else if(!prevIsPeriodicTask && apTasks[prevTaskIndex].ttc > 0) // an *A*periodic task was preempted
            {
               line << ": " << prevTaskName << " was preempted with " << apTasks[prevTaskIndex].ttc << " cycles left";
               apTasks[prevTaskIndex].state = Preempted;
               sig = true;
            }
         }
         

         if(prevTaskName == '@') // switching from "sim idle" to "active task"
         {
            line << ": Started with task " << earliestName;
            sig = true;
         }
         else if(earliestName == '+') // switching from "active task" to "sim idle"
         {
            line << ": Waiting for tasks to become available...";
            sig = true;
         }
         else // switching from one active task to another
         {
            line << ": Started task " << earliestName;
            sig = true;
         }
      }
      


      // Update idle tasks
      if(isPeriodicTask && pTasks[earliestIndex].ttc <= 0 && pTasks[earliestIndex].state != Idle)
      {
         pTasks[earliestIndex].state = Idle; // change current state
         line << ": Task " << pTasks[earliestIndex].name << " finished";
         sig = true;
      }
      else if(!isPeriodicTask && apTasks[earliestIndex].ttc <= 0 && apTasks[earliestIndex].state != Idle)
      {
         apTasks[earliestIndex].state = Idle; // change current state
         line << ": Task " << apTasks[earliestIndex].name << " finished";
         sig = true;
      }

      if(sig) // if something interesting happened during the iteration
      {
         line << endl; // send the line to the output file
         output << line.str();
      }

      prevTaskIndex = earliestIndex; // make the present the past
      prevTaskName = earliestName;
      prevIsPeriodicTask = isPeriodicTask;
   }

   output << "End of EDF Simulation" << endl;
   output.close();
   return;
}


int main(int argc, char *argv[])
{
   ifstream f;
   int num_periodic, num_aperiodic, sim_time = 0;

   f.open(argv[1], ios::in);


   if(f.good())
   {
      f >> num_periodic >> sim_time;
   }
   else
   {
      cout << "What are you pullin on me" << endl;
      return -1;
   }

   Task pTasks[num_periodic] = { };

   char name, ch;
   int e, T;
   for(int i = 0; i < num_periodic && f.good(); i++)
   {
      f >> name >> ch >> e >> ch >> T;
      Task p = Task(name, e, T, 0);
      pTasks[i] = p;
   }


   if(f.good())
   {
      f >> num_aperiodic;
   }
   else
   {
      cout << "Invalid number of Aperiodic Tasks" << endl;
      return -1;
   }

   Task aTasks[num_aperiodic] = {};

   int R;
   for(int i=0; i < num_aperiodic && f.good(); i++)
   {
      f >> name >> ch >> e >> ch >> R;
      Task q = Task(name, e, R);
      aTasks[i] = q;
   }

   f.close();

   Task* edfPTasks = copy(pTasks, num_periodic, true);
   Task* edfApTasks = copy(aTasks, num_aperiodic, false);


   rmsSchedule(pTasks, num_periodic, aTasks, num_aperiodic, sim_time, argv[2]);
   edfSchedule(edfPTasks, num_periodic, edfApTasks, num_aperiodic, sim_time, argv[2]);
   return 0;
}