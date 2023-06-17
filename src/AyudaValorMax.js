import React from 'react';

function AyudaValorMax({ value, onClick }) {
    return (
        <div
            className='booster'
            onClick={onClick}
        >
            {value}
        </div>
    )
}

export default AyudaValorMax;