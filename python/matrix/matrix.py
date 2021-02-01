class Matrix:

    rows = []
    columns = []

    def __init__(self, matrix_string):

        print("ok: ", self.rows)

        self.rows = matrix_string.split('\n')
        
       
        # self.columns = 

        print("rows: ", self.rows)
        pass

    def row(self, index):



        print("index: ", index - 1)
        print("thing: ", self.rows[index -1])
        # return self.rows[index]
        return  [int(i) for i in str(12345)]

    def column(self, index):
        pass
