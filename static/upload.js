$(document).on('click', '#basicUpload', () => {
    const fileInput = document.getElementById('upload');
    if (fileInput.files[0]) {
        
        const formData = new FormData();
        formData.append('file', fileInput.files[0]);

        const options = {
            method: 'POST',
            body: formData,
        };
        fetch('/users/upload', options)
            .then(response => {
                if (response.ok) {
                    alert('csv succesfully imported!')
                } else {
                    response.text().then( e => alert(`import unsuccessful (error: ${e})`) )
                }
            })
            .catch(e => console.log(e))
    }
});