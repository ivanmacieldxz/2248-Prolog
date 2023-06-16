import React from 'react';

function AyudaValorMax({ value, onClick }) {
    return (
        <div
            className='ayudavalormax'
            onClick={onClick}
        >
            {value}
        </div>
    )
}

export default AyudaValorMax;