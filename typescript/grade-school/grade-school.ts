
export default class GradeSchool {

    private students: string[] | undefined
    private readonly roster: Map<string, string[]> = new Map()

    studentRoster() {
        return this.roster
    }

    addStudent(_name: string, _grade: number) {

        this.students = this.roster.get('' + _grade)

        if (this.students) {
            this.students.push(_name)
            this.students.sort()
            this.roster.set('' + _grade, this.students)
        }
        else {
            this.roster.set('' + _grade, [_name])
            this.roster
        }

    }

    studentsInGrade(_grade: number): string[] {

        const students = this.roster.get('' + _grade)

        if (students)
            return Object.assign([], students);

        return []

    }

}