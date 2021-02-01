
export default class GradeSchool {

    private students: string[] | undefined
    private readonly roster: Map<number, string[]> = new Map()

    studentRoster(): Map<string, string[]> {
        const newMap = new Map()

        for (const [key, val] of this.roster.entries())
            newMap.set(key.toString(), [...val])

        return newMap
    }

    addStudent(name: string, grade: number): void {

        this.students = this.roster.get(grade)

        if (this.students) {
            this.students.push(name)
            this.students.sort()
            this.roster.set(grade, this.students)
        }
        else
            this.roster.set(grade, [name])

    }

    studentsInGrade(grade: number): string[] {

        const students = this.roster.get(grade) || []

        return [...students]

    }

}