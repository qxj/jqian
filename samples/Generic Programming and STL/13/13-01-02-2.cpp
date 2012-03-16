#include <iostream>
#include <algorithm>
#include <vector>
#include <iterator>
#include <string>
using namespace std;

class job {
public:
  enum priority_code {standby, normal, high, urgent};
  job(string id, priority_code p = normal): nam(id), pri(p) {}
  
  string name() const { return nam; }
  priority_code priority() const { return pri; }

private:
  string nam;
  priority_code pri;
};

ostream& operator<<(ostream& os, const job& j) {
  os << j.name() << " (" << j.priority() << ")";
  return os;
}

bool operator<(const job& j1, const job& j2) {
  return j1.priority() > j2.priority();
}

int main() {
  vector<job> jobs;
  
  jobs.push_back(job("Long computation", job::standby));
  jobs.push_back(job("System reboot", job::urgent));
  jobs.push_back(job("Print"));
  jobs.push_back(job("Another long computation", job::standby));
  jobs.push_back(job("Copy file"));
  
  stable_sort(jobs.begin(), jobs.end());
  copy(jobs.begin(), jobs.end(), ostream_iterator<job>(cout, "\n"));
}
